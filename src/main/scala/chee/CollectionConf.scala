package chee

import scala.util.Try
import com.typesafe.scalalogging.LazyLogging

final class CollectionConf(config: => ConfigFile) extends LazyLogging {

  def list: Try[Seq[Collection]] = Try {
    config.getOpt[Seq[Collection]]("chee.collections") getOrElse Seq.empty[Collection]
  }

  def find(name: String): Try[Option[Collection]] =
    list.map(_.filter(_.name.startsWith(name)) match {
      case c :: Nil => Some(c)
      case _ => None
    })

  def get(name: String): Try[Option[Collection]] =
    list.map(_.find(_.name == name))

  def add(e: Collection): Try[Unit] = addAll(Seq(e))

  def addAll(es: Seq[Collection]): Try[Unit] = Try {
    logger.trace(s"""Add collections $es to config""")
    val existing = list.get
    val existsName = existing.map(_.name).toSet
    val toadd = es.foldLeft(existing) { (list, e) =>
      if (existsName(e.name)) list.map(x => if (x.name == e.name) e else x)
      else list :+ e
    }
    setNewContents(toadd)
  }

  def replace(name: String, coll: Collection): Try[Unit] = Try {
    val colls = list.get.filter(_.name != name) :+ coll
    setNewContents(colls)
  }

  def remove(name: String) = Try {
    logger.trace(s"""Remove ${name} from collections""")
    val entries = list.get.filter(_.name != name)
    setNewContents(entries)
    entries
  }

  def deleteAll: Try[Unit] = {
    Try(setNewContents(Seq()))
  }

  private def setNewContents(es: Seq[Collection]): Unit = {
    config.set("chee.collections", es).save()
  }
}

case class Collection(
  name: String,
  query: String,
  description: String = "") {

  lazy val title = description.split("\r?\n").head
}

object Collection {
  import ConfigFile.Conversion
  import ConfigFile.Conversion._
  import com.typesafe.config.ConfigObject
  import chee.properties._
  import scala.language.implicitConversions

  implicit val entryConv: Conversion[Collection] = Conversion(
    coll => fromMap.make(Map(
      "name" -> coll.name,
      "query" -> coll.query,
      "description" -> coll.description
    )),
    cv => {
      val cfg = cv.asInstanceOf[ConfigObject].toConfig()
      Collection(
        name = cfg.getString("name"),
        query = cfg.getString("query"),
        description = cfg.getString("description"))
    }
  )

  implicit def asMap(coll: Collection): LazyMap = LazyMap(
    Ident("name") -> coll.name,
    Ident("query") -> coll.query,
    Ident("title") -> coll.title,
    Ident("description") -> coll.description)

}
