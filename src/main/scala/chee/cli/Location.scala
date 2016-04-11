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

  /** Test whether `f` is inside a location given by `locations`.
    *
    * Return the location that `f` is a child of, or `None`.
    */
  private def findFileLocation(locations: Set[File])(f: File): Option[File] =
    locations.find(l => f.path.startsWith(l.path))

  /** Filter a list of directories by whether they are childs of known
    * locations.
    *
    * Apply the `include` function to the result of `findFileLocation`
    * and if `true` include dir (from `dirs`) in the result.
    */
  private def filterFileLocation(conf: chee.LocationConf, include: Option[File] => Boolean)(dirs: Seq[File]): Seq[File] = {
    val existing = conf.list.map(_.map(_.dir).toSet).get
    val check = findFileLocation(existing)_
    dirs.filter(d => include(check(d)))
  }

  private def checkFileLocation(conf: chee.LocationConf, msg: String, err: Option[File] => Boolean, dirs: Seq[File]): Unit = {
    val failedDirs = filterFileLocation(conf, err)(dirs)
    if (failedDirs.isEmpty) ()
    else userError(failedDirs.map(d => s"`${d.path}' $msg").mkString("\n"))
  }

  /** Check if all `dirs` are known locations (or childs thereof). */
  def checkRegisteredLocations(conf: chee.LocationConf, dirs: Seq[File]): Unit =
    checkFileLocation(conf, "is not a known location", _.isEmpty, dirs)

  /** Check if all `dirs` are not known locations. */
  def checkNotRegisteredLocations(conf: chee.LocationConf, dirs: Seq[File]): Unit =
    checkFileLocation(conf, "is a known location", _.nonEmpty, dirs)
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
    } textW ("One or many directories that are deleted from the index and location set.")
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
