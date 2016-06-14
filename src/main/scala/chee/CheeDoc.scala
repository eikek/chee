package chee

import chee.properties.{ Ident, LazyMap, Patterns }
import java.net.URL
import com.typesafe.config.Config

import scala.io.Source
import scala.util.Try

import better.files._
import chee.conf._

object CheeDoc {

  def findPage(name: String, format: String): Option[URL] =
    Option(getClass.getResource(s"/chee/doc/$format/$name.$format"))

  def findCommandPage(cmd: String, format: String): Option[URL] =
    findPage(s"cmd-$cmd", format)

  def findAboutPage(cmd: String, format: String): Option[URL] =
    findPage(s"about-$cmd", format)

  def openPage(cfg: Config)(name: String): Try[Unit] = Try {
    val target = cfg.getFile("chee.tmpdir") / (name +".html")
    copyPage(name, "html", target) getOrElse {
      sys.error(s"The page ${name} could not be found!")
    }
    OS(cfg).browse(target)
  }

  /** Opens the html page for the given command in a browser */
  def openCommandPage(cfg: Config)(cmd: String): Try[Unit] =
    openPage(cfg)("cmd-"+cmd)

  def openAboutPage(cfg: Config)(name: String): Try[Unit] =
    openPage(cfg)("about-"+name)

  def openManual(cfg: Config): Try[Unit] =
    openPage(cfg)("manual")

  /** Return the first paragraph of the command help page. */
  def commandSummaryLine(cmd: String, subcmd: Option[String] = None): Option[String] = {
    implicit val codec = io.Codec.UTF8
    def advance(lines: Iterator[String]): Option[Iterator[String]] = subcmd match {
      case None => Some(lines)
      case Some(sub) =>
        lines.find(_.startsWith(s"==== $sub")).map(_ => lines)
    }

    findCommandPage(cmd, "adoc")
      .map(Source.fromURL)
      .map(_.getLines)
      .flatMap(advance)
      .map(firstParagraph)
  }

  def formatCommandSummary(cmds: List[List[String]]): String = {
    import Patterns._
    val (maxlen, summary) = cmds.sortBy(_.mkString).foldRight((0, List[LazyMap]())) {
      case (cmd, (len, sum)) => cmd match {
        case cmd =>
          val name = cmd.mkString(" ")
          val text = commandSummaryLine(cmd.head, cmd.tail.headOption)
            .getOrElse("No documentation available.")
          (math.max(len, name.length), LazyMap(Ident("name") -> name, Ident("text") -> text) :: sum)
      }
    }
    val restlen = 70 - 3 - maxlen
    val wrapText = lookup('text).rmap { text =>
      cli.wrapLines(restlen)(text).split("\n").mkString(("\n" :: List.fill(maxlen + 3)(" ")).mkString)
    }
    val format = seq(
      fixedwidth(maxlen, readable('name)),
      raw(" - "),
      wrapText)

    def unwrap(in: Either[String, String]): String = in match {
      case Right(s) => s
      case Left(m) => sys.error(m)
    }

    summary.map(format.result).map(unwrap).mkString("\n")
  }

  private[chee] def firstParagraph(lines: Iterator[String]): String = {
    @scala.annotation.tailrec
    def loop(emptyLines: Int = 0, result: StringBuilder = new StringBuilder): String =
      if (lines.hasNext) lines.next match {
        case _ if emptyLines >= 2 => result.toString
        case "" => loop(emptyLines + 1, result)
        case _ if emptyLines == 0 => loop(emptyLines, result)
        case line => loop(emptyLines, result append " " append line.trim)
      } else result.toString

    loop().trim
  }

  /** Copies a help page to the given location.
    *
    * If the name of `target` has an extension `format` write the help
    * page in this file, overwriting existing content.
    *
    * Otherwise, consider `target` a directory and copy the help page
    * into it.
    *
    * Directories will be created if necessary.
    *
    * Returns the new file or `None` if the help page was not found.
    */
  def copyPage(name: String, format: String, target: File): Option[File] = {
    for (page <- findPage(name, format)) yield {
      val file =
        if (!target.isDirectory && target.name.endsWith("."+format)) target
        else target / (name +"."+ format)
      file.createIfNotExists(createParents = true).clear()
      Source.fromURL(page).getLines.foreach(file.appendLine)
      file
    }
  }
}
