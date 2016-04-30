package chee.cli

import com.typesafe.config.Config
import scala.io.Source

import chee.CheeDoc
import chee.doc.CheeDocInfo
import Help.Opts

class Help extends ScoptCommand {

  type T = Opts

  val name = "help"
  val defaults = Opts(name = name)

  val commands = CheeDocInfo.docFiles.filter(_.startsWith("cmd-"))
    .map(_.replaceAll("\\.adoc", ""))

  val topics =  CheeDocInfo.docFiles.filter(_.startsWith("about-"))
    .map(_.replaceAll("\\.adoc", ""))

  val parser = new Parser {
    opt[Unit]("html") action { (_, c) =>
      c.copy(format = "html")
    } text ("Show the manual page in html (opens the browser).")

    opt[Unit]('c', "command") optional() action { (_, c) =>
      c.copy(prefix = "cmd")
    } text ("See help for a command")

    opt[Unit]('a', "about") optional() action { (_, c) =>
      c.copy(prefix = "about")
    } text("See help about a concept topic.")

    arg[String]("<name>") optional() action { (n, c) =>
      c.copy(name = n)
    } textW (
      "The name of a page or command; or `manual' to open the complete manual in a browser."
    )
  }

  private def prefixFind(n: String, cand: List[String]): String =
    cand.filter(_.startsWith(n)) match {
      case c :: Nil => c
      case c :: cs =>
        chee.UserError(s"""Ambiguous name $n, possible commands are: ${cand.mkString(", ")}""")
      case Nil =>
        chee.UserError(s"Help not found for `$n'")
    }

  def exec(cfg: Config, opts: Opts): Unit = opts match {
    case Opts(_, _, "manual") =>
      outln("Opening the manual...")
      CheeDoc.openManual(cfg).get
    case Opts(prefix, "adoc", name) =>
      val cand = if (prefix == "cmd") commands else topics
      CheeDoc.findPage(prefixFind(prefix+"-"+name, cand), "adoc").map(Source.fromURL) match {
        case Some(src) =>
          src.getLines.foreach(outln)
        case None =>
          errln(s"""No help for ${if (prefix == "cmd") "command" else "about page"} '$name' available.""")
      }
    case Opts(prefix, "html", name) =>
      outln(s"Opening page $name ...")
      val cand = if (prefix == "cmd") commands else topics
      CheeDoc.openPage(cfg)(prefixFind(prefix+"-"+name, cand)).get
  }
}

object Help {
    case class Opts(
    prefix: String = "cmd",
    format: String = "adoc",
    name: String)
}
