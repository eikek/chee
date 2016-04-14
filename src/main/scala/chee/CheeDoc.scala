package chee

import java.net.URL
import com.typesafe.config.Config

import scala.io.Source
import scala.sys.process.Process
import scala.util.Try

import better.files._
import chee.CheeConf.Implicits._

object CheeDoc {

  def findPage(name: String, format: String): Option[URL] =
    Option(getClass.getResource(s"/chee/doc/$format/$name.$format"))

  def findCommandPage(cmd: String, format: String): Option[URL] =
    findPage(s"cmd-$cmd", format)

  def findAboutPage(cmd: String, format: String): Option[URL] =
    findPage(s"about-$cmd", format)

  def openPage(cfg: Config)(name: String): Try[Int] = Try {
    val target = cfg.getFile("chee.tmpdir") / (name +".html")
    copyPage(name, "html", target) getOrElse {
      sys.error(s"The page ${name} could not be found!")
    }
    browse(cfg, target).get
  }

  /** Opens the html page for the given command in a browser */
  def openCommandPage(cfg: Config)(cmd: String): Try[Int] =
    openPage(cfg)("cmd-"+cmd)

  def openAboutPage(cfg: Config)(name: String): Try[Int] =
    openPage(cfg)("about-"+name)

  def openManual(cfg: Config): Try[Int] =
    openPage(cfg)("manual")


  private def browse(cfg: Config, file: File): Try[Int] = Try {
    val cmd = (cfg.getCommand("chee.programs.browser").collect {
      case "%s" => file.path.toString
      case s => s
    }).toSeq
    Process(cmd(0), cmd.drop(1)).!
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
      file.createIfNotExists().clear()
      Source.fromURL(page).getLines.foreach(file.appendLine)
      file
    }
  }
}
