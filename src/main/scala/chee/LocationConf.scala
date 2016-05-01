package chee

import better.files._
import scala.util.{Try, Success, Failure}
import com.typesafe.scalalogging.LazyLogging
import LocationConf.Entry
import chee.util.paths

final class LocationConf(config: => ConfigFile, root: Option[File]) extends LazyLogging {

  private def readFile: Try[Seq[Entry]] = Try {
    config.getOpt[Seq[Entry]]("chee.locations") getOrElse Seq.empty[Entry]
  }

  def list: Try[Seq[Entry]] = Try {
    val entries = readFile.get
    root match {
      case Some(dir) => entries.map(_.resolveTo(root))
      case _ => entries
    }
  }

  def add(e: Entry): Try[Unit] = addAll(Seq(e))

  def addAll(es: Seq[Entry]): Try[Unit] = Try {
    logger.trace(s"""Add locations $es to location config""")
    val existing = readFile.get
    val existsDir = existing.map(_.dirname).toSet
    val toadd = es.map(_.relativeTo(root)).foldLeft(existing) { (list, e) =>
      if (existsDir(e.dirname)) list.map(x => if (x.dir == e.dir) e else x)
      else list :+ e
    }
    setNewContents(toadd)
  }

  def remove(dir: File) = Try {
    logger.trace(s"""Remove ${dir.path} from location config""")
    val entries = list.get.filter(_.dir != dir).map(_.relativeTo(root))
    setNewContents(entries)
    entries
  }

  def deleteAll: Try[Unit] = {
    Try(setNewContents(Seq()))
  }

  private def setNewContents(es: Seq[Entry]): Unit = {
    config.set("chee.locations", es).save()
  }
}

object LocationConf {
  import ConfigFile.Conversion
  import ConfigFile.Conversion._
  import com.typesafe.config.ConfigObject

  case class Entry(
    dirname: String,
    query: String = "",
    recursive: Boolean = false,
    all: Boolean = false) {

    lazy val dir = File(dirname)

    def relativeTo(root: Option[File]) = root match {
      case Some(rootDir) =>
        copy(dirname = paths.relative(rootDir)(dir).toString)
      case _ => this
    }

    def resolveTo(root: Option[File]) = root match {
      case Some(rootDir) =>
        copy(dirname = paths.resolve(rootDir)(dirname))
      case _ => this
    }

    def changeDirTo(f: File) = copy(dirname = f.pathAsString)
  }

  object Entry {
    def apply(dir: File, query: String, recursive: Boolean, all: Boolean): Entry =
      Entry(dir.pathAsString, query, recursive, all)
  }

  implicit val entryConv: Conversion[Entry] = Conversion(
    entry => fromMap.make(Map(
      "dir" -> entry.dirname,
      "query" -> entry.query,
      "recursive" -> entry.recursive,
      "all" -> entry.all
    )),
    cv => {
      val cfg = cv.asInstanceOf[ConfigObject].toConfig()
      Entry(
        dirname = cfg.getString("dir"),
        query = cfg.getString("query"),
        recursive = cfg.getBoolean("recursive"),
        all = cfg.getBoolean("all"))
    }
  )
}
