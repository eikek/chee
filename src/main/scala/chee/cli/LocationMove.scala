package chee.cli

import com.typesafe.config.Config
import better.files._
import chee.query._
import chee.CheeConf.Implicits._
import com.typesafe.scalalogging.LazyLogging

object LocationMove extends ScoptCommand with LockSupport {
  import java.time.Duration
  import chee.LocationConf
  import chee.LocationConf.Entry

  type T = Opts

  val name = "mv"
  val defaults = Opts()

  case class Opts(
    indexOnly: Boolean = false,
    src: File = file"",
    target: File = file"")

  val parser = new Parser {
    opt[Unit]("index") optional() action { (_, c) =>
      c.copy(indexOnly = true)
    } text ("Only update the index but don't move the directory.")

    arg[File]("<src>") required() action { (x, c) =>
      c.copy(src = x)
    } text ("The source location.")

    arg[File]("<target>") required() action { (x, c) =>
      c.copy(target = x)
    } text ("The target location.")
  }

  def exec(cfg: Config, opts: Opts): Unit = withLock(cfg) {
    Location.checkRegisteredLocations(cfg.getLocationConf, Seq(opts.src))
    if (opts.src.exists && !opts.indexOnly) {
      opts.src.moveTo(opts.target)
    }

    val loc = cfg.getLocationConf.list.get.find(_.dir == opts.src).get
    cfg.getLocationConf.remove(opts.src).get
    cfg.getLocationConf.add(loc.copy(dir = opts.target))

    val sqlite = new SqliteBackend(cfg.getIndexDb)
    out(s"Move `${opts.src.path}' → `${opts.target.path}' … ")
    val n = sqlite.changeLocation(opts.src.path, opts.target.path).get
    outln(s"changed $n entries")
  }
}
