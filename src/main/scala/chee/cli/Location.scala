package chee.cli

import better.files._
import chee.conf._
import chee.properties._
import chee.query._
import chee.util.files._
import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging

object Location {
  val root = HubCommand("location", List(
    new LocationAdd,
    new LocationUpdate,
    new LocationImport,
    new LocationInfo,
    new LocationSync))

  /** Test whether `f` is inside a location given by `locations`.
    *
    * Return the location that `f` is a child of, or `None`.
    */
  private def findFileLocation(locations: Set[File])(f: File): Option[File] =
    locations.find(_ parentOf f)

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

  def checkRepoRoot(conf: Config, dirs: Seq[File]): Unit =
    conf.getRepoRoot match {
      case Some(root) if conf.getBoolean("chee.repo.restrict-to-root") =>
        val errors = dirs.filterNot(f => root.isParentOf(f))
        if (errors.nonEmpty) {
          userError(s"""Directories ${errors.mkString(", ")} outside of repository root ${root.path}!""")
        }
      case _ =>
    }
}

class LocationInfo extends Command {
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
