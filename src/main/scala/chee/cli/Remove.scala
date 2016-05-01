package chee.cli

import Remove._
import better.files._
import chee.query.SqliteBackend
import com.typesafe.config.Config
import chee.conf._
import chee.properties._
import scala.util.Try

class Remove extends ScoptCommand with LockSupport {

  type T = Opts

  val name = Remove.name
  val defaults = Opts()

  val parser = new Parser {
    opt[Unit]("index") optional() action { (_, c) =>
      c.copy(indexOnly = true)
    } textW ("Only update the index but don't remove any files.")

    arg[File]("<files...>") unbounded() required() action { (x, c) =>
      c.copy(files = c.files :+ x)
    } textW ("The files or directories to remove. Directories are removed recursively.")
  }

  def exec(cfg: Config, opts: Opts): Unit = withLock(cfg) {
    Location.checkRegisteredLocations(cfg.getLocationConf, opts.files)

    val n = opts.files.foldLeft(0) { (i, f) =>
      out(s"Remove ${f.path} … ")
      val n = remove(cfg, f, opts.indexOnly).get
      outln("ok")
      i + n
    }
    outln(s"Removed $n entries")
  }
}

object Remove {
  val name = "rm"

  case class Opts(
    indexOnly: Boolean = false,
    files: Seq[File] = Seq.empty)

  def remove(cfg: Config, file: File, indexOnly: Boolean): Try[Int] = Try {
    val sqlite = new SqliteBackend(cfg)
    val n = sqlite.delete(Prop(Comp.Like, Ident.path -> s"${file.pathAsString}*")).get
    if (cfg.getLocationConf.list.get.exists(_.dir == file)) {
      cfg.getLocationConf.remove(file).get
    }
    if (file.exists && !indexOnly) {
      file.delete()
    }
    n
  }
}
