package chee.cli

import com.typesafe.config.Config
import chee.CheeConf.Implicits._
import chee.Collection
import chee.properties._
import chee.properties.Patterns._

object CollectionList extends ScoptCommand {

  val name = "list"

  case class Opts(
    pattern: Pattern = lookup('name)
  )

  type T = Opts

  val defaults = Opts()

  val parser = new CheeOptionParser[Opts](name) {
    opt[Unit]('d', "description") action { (_, c) =>
      c.copy(pattern = seq(lookup('name), raw(" – "), lookup('description)))
    } text ("Show the description title.")
  }

  def makeMap(coll: Collection) =
    LazyMap(
      Ident("name") -> coll.name,
      Ident("description") -> coll.description.split("\r?\n").head)

  def exec(cfg: Config, opts: Opts): Unit = {
    val conf = cfg.getCollectionConf
    conf.list.get.map(makeMap) foreach { m =>
      outln(opts.pattern.right(userError).result(m))
    }
  }
}
