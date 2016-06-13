package chee.cli

import CollectionRemove._
import com.typesafe.config.Config
import chee.conf._

class CollectionRemove extends ScoptCommand {

  type T = Opts
  val defaults = Opts()
  val name = "remove"

  val parser = new Parser {
    arg[String]("<name>") required() action { (n, c) =>
      c.copy(name = n)
    } textW ("The name of the collection to remove.")
  }

  def exec(cfg: Config, opts: Opts): Unit = {
    val conf = cfg.getCollectionConf
    conf.find(opts.name).get match {
      case Some(coll) =>
        conf.remove(coll.name)
        outln(s"Removed collection ${opts.name}")
      case _ =>
        errln(s"Collection ${opts.name} not found.")
    }
  }
}

object CollectionRemove {
  case class Opts(name: String = "")
}
