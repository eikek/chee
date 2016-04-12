package chee.cli

import com.typesafe.config.Config
import better.files._
import chee.properties._
import chee.query._
import chee.CheeConf.Implicits._
import com.typesafe.scalalogging.LazyLogging

object LocationAdd extends ScoptCommand with LockSupport {
  import java.time.Duration
  import chee.LocationConf.Entry
  import chee.Timing
  import chee.Timing.timed

  type T = Opts

  val name = "add"
  val defaults = Opts()

  case class Opts(
    recursive: Boolean = false,
    all: Boolean = false,
    query: String = "",
    dirs: Seq[File] = Seq.empty)

  val parser = new Parser {
    opt[Unit]('r', "recursive") optional() action { (_, c) =>
      c.copy(recursive = true)
    } text ("Find files recursively.")

    opt[Unit]('a', "all") optional() action { (_, c) =>
      c.copy(all = true)
    } text ("Ignore the default query.")

    opt[String]('q', "query") optional() action { (q, c) =>
      c.copy(query = q)
    } textW ("The query string. See the manual page about queries for more information.")

    arg[Seq[File]]("<directories>") unbounded() action { (x, c) =>
      c.copy(dirs = c.dirs ++ x)
    } validate { dirs =>
      dirs.find(d => !d.isDirectory) match {
        case Some(dir) => failure(s"Path `${dir.path}' is not a directory (or does not exist)")
        case _ => success
      }
    } text ("One or more existing directories.")
  }

  case class ProgressCount(added: Int = 0, existed: Int = 0) {
    def incAdded = copy(added = added + 1)
    def incExisted = copy(existed = existed + 1)
  }

  val progress = Progress.seq[Boolean, ProgressCount](
    Progress.before {
      MapGet.value(Ident.path).map { path =>
        out(s"Adding ${path.get} … ")
      }
    },
    Progress.after { (data, added, dur) =>
      if (added) {
        outln("ok")
        data.incAdded
      } else {
        outln("exists")
        data.incExisted
      }
    },
    Progress.done { (data, dur) =>
      outln(s"Added ${data.added}, skipped ${data.existed} files in ${Timing.format(dur)}")
    }
  )

  def fileCondition(opts: Opts, cfg: Config): Condition = {
    val query = cfg.makeQuery
    AbstractLs.getFileCondition(query, opts.query, cfg, opts.all) match {
      case Right(cond) => cond
      case Left(msg) => chee.UserError(msg)
    }
  }

  def indexDirs(cfg: Config, opts: Opts, sqlite: SqliteBackend): Unit = {
    val cond = fileCondition(opts, cfg)
    val added = Extraction.added(DateTime.now)
    for (dir <- opts.dirs) {
      cfg.getLocationConf.add(Entry(dir, opts.query, opts.recursive, opts.all)).get
      val files = FileBackend.find(cond, dir, opts.recursive).map(_ +  added)
      sqlite.insert(files, ProgressCount(), progress)
    }
  }

  def exec(cfg: Config, opts: Opts): Unit = withLock(cfg) {
    Location.checkNotRegisteredLocations(cfg.getLocationConf, opts.dirs)
    val sqlite = new SqliteBackend(cfg.getIndexDb)
    indexDirs(cfg, opts, sqlite)
  }
}
