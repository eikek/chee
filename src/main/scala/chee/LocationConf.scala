package chee

import better.files._
import scala.util.{Try, Success, Failure}
import com.typesafe.scalalogging.LazyLogging

final class LocationConf(config: => ConfigFile) extends LazyLogging {
  import LocationConf.Entry

  def list: Try[Seq[Entry]] = Try {
    config.getOpt[Seq[Entry]]("chee.locations") getOrElse Seq.empty[Entry]
  }

  def add(e: Entry): Try[Unit] = addAll(Seq(e))

  def addAll(es: Seq[Entry]): Try[Unit] = Try {
    logger.trace(s"""Add locations $es to location config""")
    val existing = list.get
    val existsDir = existing.map(_.dir).toSet
    val toadd = es.foldLeft(existing) { (list, e) =>
      if (existsDir(e.dir)) list.map(x => if (x.dir == e.dir) e else x)
      else list :+ e
    }
    setNewContents(toadd)
  }

  def remove(dir: File) = Try {
    logger.trace(s"""Remove ${dir.path} from location config""")
    val entries = list.get.filter(_.dir != dir)
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
    dir: File,
    query: String = "",
    recursive: Boolean = false,
    all: Boolean = false)

  implicit val entryConv: Conversion[Entry] = Conversion(
    entry => fromMap.make(Map(
      "dir" -> entry.dir.path.toString,
      "query" -> entry.query,
      "recursive" -> entry.recursive,
      "all" -> entry.all
    )),
    cv => {
      val cfg = cv.asInstanceOf[ConfigObject].toConfig()
      Entry(
        dir = File(cfg.getString("dir")),
        query = cfg.getString("query"),
        recursive = cfg.getBoolean("recursive"),
        all = cfg.getBoolean("all"))
    }
  )
}
