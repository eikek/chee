package chee.cli

import com.typesafe.config.Config
import chee.CheeConf.Implicits._
import chee.Collection
import chee.properties._
import chee.properties.Patterns._

object CollectionShow extends ScoptCommand {

  val name = "show"

  case class Opts(
    name: String = ""
  )

  type T = Opts

  val defaults = Opts()

  val parser = new CheeOptionParser[Opts](name) {
    arg[String]("<name>") required() action { (n, c) =>
      c.copy(name = n)
    } text ("The collection name or enough of it to uniquely identify a\n"+
      "        collection.")
  }

  def makeMap(coll: Collection) =
    LazyMap(
      Ident("name") -> coll.name,
      Ident("query") -> coll.query,
      Ident("title") -> coll.description.split("\r?\n").head,
      Ident("description") -> coll.description)

  val pattern = seq(
    raw("* "), lookup('title), raw(" ("), lookup('name), raw(")"), newline,
    raw("** Query"), newline, newline,
    lookup('query), newline, newline,
    raw("** Description"), newline, newline,
    lookup('description)
  )

  def exec(cfg: Config, opts: Opts): Unit = {
    cfg.getCollectionConf.find(opts.name).get match {
      case Some(coll) =>
        outln(pattern.right(userError).result(makeMap(coll)))
      case _ =>
        errln(s"Collection not found: ${opts.name}")
    }
  }
}
