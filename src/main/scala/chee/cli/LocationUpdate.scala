package chee.cli

import com.typesafe.config.Config
import chee.conf._
import better.files._
import chee.query._
import LocationUpdate._

class LocationUpdate extends ScoptCommand with LockSupport {

  type T = Opts

  val name = "update"
  val defaults = Opts()

  val parser = new Parser {
    opt[Unit]("all") optional() action { (_, c) =>
      c.copy(all = true)
    } text ("Update all locations.")

    arg[Seq[File]]("<directories>") unbounded() optional() action { (x, c) =>
      c.copy(dirs = c.dirs ++ x)
    } validate { dirs =>
      dirs.find(d => !d.isDirectory) match {
        case Some(dir) => failure(s"Path `${dir.path}' is not a directory (or does not exist)")
        case _ => success
      }
    } textW ("One or more directories to update. They must be registered locations.")

    checkConfig { opts =>
      if (opts.dirs.isEmpty && !opts.all) failure("Use --all to update all known locations.")
      else if (opts.dirs.nonEmpty && opts.all) failure("Do not use --all when also specifying locations.")
      else success
    }
  }

  def update(cfg: Config, opts: Opts): Unit = {
    Location.checkRegisteredLocations(cfg.getLocationConf, opts.dirs)
    val locs = cfg.getLocationConf.list.get match {
      case l if opts.all => l
      case l => l.filter(e => opts.dirs.contains(e.dir))
    }
    val sqlite = new SqliteBackend(cfg.getIndexDb)
    for (locEntry <- locs) {
      val addOpts = LocationAdd.Opts().copy(
        dirs = Seq(locEntry.dir),
        recursive = locEntry.recursive,
        all = locEntry.all,
        query = locEntry.query)
      new LocationAdd().indexDirs(cfg, addOpts, sqlite)
    }
  }

  def exec(cfg: Config, opts: Opts): Unit = withLock(cfg) {
    update(cfg, opts)
  }
}

object LocationUpdate {
  case class Opts(
    all: Boolean = false,
    dirs: Seq[File] = Seq.empty)
}
