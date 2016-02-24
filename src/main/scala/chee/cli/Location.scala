package chee.cli

import com.typesafe.config.Config
import better.files._
import chee.properties._
import chee.query._
import chee.CheeConf.Implicits._
import com.typesafe.scalalogging.LazyLogging

object Location {
  val root = HubCommand("location", List(
    LocationAdd,
    LocationUpdate,
    LocationDelete,
    LocationImport,
    LocationInfo,
    LocationSync,
    LocationMove))

  private def compareFiles(conf: chee.LocationConf, msg: String, f: (Set[File], Set[File]) => Set[File], dirs: Seq[File]): Boolean = {
    val given = dirs.toSet
    val existing = conf.list.map(_.map(_.dir).toSet)
    existing.map(ex => f(given, ex) match {
      case s if s.isEmpty => true
      case s if s.size == 1 => chee.UserError(s"`${s.head}' $msg")
      case s => chee.UserError(s.map("`"+ _ +"'").mkString("", s" $msg\n", s" $msg"))
    }).get
  }

  def checkRegisteredLocations(conf: chee.LocationConf, dirs: Seq[File]): Unit =
    compareFiles(conf, "is not a known location", _ diff _, dirs)

  def checkNotRegisteredLocations(conf: chee.LocationConf, dirs: Seq[File]): Unit =
    compareFiles(conf, "is a known location", _ intersect _, dirs)
}

object LocationInfo extends Command {
  val name = "info"

  def exec(cfg: Config, args: Array[String]): Unit = {
    val sqlite = new SqliteBackend(cfg.getIndexDb)
    val file = cfg.getLocationConf

    for (loc <- file.list.get) {
      val count = sqlite.count(Prop(Comp.Eq, Ident.location -> loc.dir.path.toString)).get
      outln(s"${loc.dir}: $count")
    }
    outln(s"All: ${sqlite.count(TrueCondition).get}")
  }
}

object LocationDelete extends ScoptCommand with LockSupport {
  type T = Opts

  val name = "delete"
  val defaults = Opts()

  case class Opts(
    all: Boolean = false,
    dirs: Seq[File] = Seq.empty)

  val parser = new CheeOptionParser[Opts](name) {
    opt[Unit]("all") optional() action { (_, c) =>
      c.copy(all = true)
    } text("Remove all locations.")

    arg[Seq[File]]("<directories>") optional() unbounded() action { (x, c) =>
      c.copy(dirs = c.dirs ++ x)
    } text ("One or many directories that are deleted from the index and location\n        set.")
  }

  def exec(cfg: Config, opts: Opts): Unit = withLock(cfg) {
    Location.checkRegisteredLocations(cfg.getLocationConf, opts.dirs)
    val file = cfg.getLocationConf
    val sqlite = new SqliteBackend(cfg.getIndexDb)
    if (opts.all) {
      val count = sqlite.delete(TrueCondition).get
      file.deleteAll.get
      outln(s"$count files deleted from index")
    } else {
      val locs = file.list.get
      opts.dirs.find(f => !locs.map(_.dir).contains(f)) match {
        case Some(f) => chee.UserError(s"`$f' is not a known location")
        case _ =>
      }
      for (dir <- opts.dirs) {
        val count = sqlite.delete(Prop(Comp.Eq, Ident.location -> dir.path.toString)).get
        file.remove(dir)
        outln(s"${dir.path}: $count files deleted from index")
      }
    }
  }
}
