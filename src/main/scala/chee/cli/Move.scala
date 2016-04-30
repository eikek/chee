package chee.cli

import scala.util.Try

import Move.{Opts, SrcDest}
import better.files._
import chee.LocationConf
import chee.query.SqliteBackend
import chee.conf._
import chee.util.files._
import com.typesafe.config.Config

class Move extends ScoptCommand with LockSupport {

  type T = Opts

  val name = "mv"
  val defaults = Opts()

  val parser = new Parser {
    opt[Unit]("index") optional() action { (_, c) =>
      c.copy(indexOnly = true)
    } textW ("Only update the index but don't move the directory.")

    arg[File]("<files...>") unbounded() required() action { (x, c) =>
      c.copy(files = c.files :+ x)
    } textW ("The files or directories to move. The last is the target. "+
      "The target must be a directory if multiple sources are specified.")

    checkConfig {
      case Opts(_, Nil) =>
        failure("No files or directories to move.")
      case Opts(_, a :: Nil) =>
        failure(s"Target is missing. Only ${a.path} specified.")
      case SrcDest(src, _) if !src.forall(_.exists) =>
        failure(s"""The source does not exist: ${src.filterNot(_.exists).mkString(", ")}""")
      case SrcDest(_ :: _ :: _, trg) if !trg.isDirectory =>
        failure(s"Multiple source files require an existing target directory: ${trg.path} does not exist or is not a directory.")
      case SrcDest(src, trg) if src contains trg =>
        failure("Source and target are the same file.")
      case SrcDest(src, trg) if src.exists(s => trg.isChildOf(s)) =>
        failure("Cannot move a file into a sub directory of itself: "+
          src.filter(s => trg.isChildOf(s)).mkString(", "))
      case _ =>
        success
    }
  }

  def exec(cfg: Config, opts: Move.Opts): Unit = withLock(cfg) {
    val sources = Move.validateSources(cfg, opts.sources)
    val n = sources.foldLeft(0) { (i, src) =>
      val target = Move.resolveTarget(src, opts.target)
      out(s"Move `${src.path}' → `${target.path}' … ")
      val n = Move.move(cfg, src, target, opts.indexOnly).get
      outln(" ok")
      i + n
    }
    outln(s"Updated $n entries")
  }
}

object Move {
  case class Opts(
    indexOnly: Boolean = false,
    files: List[File] = Nil) {

    lazy val (sources, target) = files.reverse match {
      case a :: as => (as.reverse, a)
      case _ => sys.error("not enough files")
    }
  }

  object SrcDest {
    def unapply(opts: Opts): Option[(List[File], File)] =
      Some((opts.sources, opts.target))
  }

  def resolveTarget(source: File, target: File) = target match {
    case t if !t.exists =>
      target.parent.createDirectories()
      t
    case t if !t.isDirectory =>
      userError("The target must be a directory or should not exist.")
    case t =>
      val f = t / source.name
      if (f.exists) userError(s"The target ${f.path} already exists.")
      else f
    }

  def validateSources(cfg: Config, sources: Seq[File]): Seq[File] = {
    Location.checkRegisteredLocations(cfg.getLocationConf, sources)
    sources.filter(_.notExists) match {
      case x if x.nonEmpty =>
        userError(s"""The following sources do not exist: ${x.map(_.path).mkString(", ")}""")
      case _ =>
    }
    sources
  }

  private def findLocations(cfg: Config, source: File, target: File): (LocationConf.Entry, Option[LocationConf.Entry]) = {
    val locations = cfg.getLocationConf.list.get
    val sourceLocation = locations.find(l => source.isChildOf(l.dir)) match {
      case Some(x) => x
      case None => sys.error("unreachable code, this condition must be checked beforehand")
    }
    val targetLocation = locations.find(_.dir parentOf target)
    if (targetLocation.isEmpty) {
      if (!locations.map(_.dir).contains(source))
        userError(s"Cannot move `${source.path}' outside a location.")
    }

    (sourceLocation, targetLocation)
  }

  /** Move {{source}} to {{target}} and update the index
    * database. {{target}} must not exist (use
    * {{resolveTarget}}). {{source}} must exist and inside a location,
    * it may be a file or directory. If {{indexOnly == true}} the
    * file(s) are not moved but only the database is updated. Return
    * the number of updated database entries. */
  def move(cfg: Config, source: File, target: File, indexOnly: Boolean = false): Try[Int] = Try {
    val (sourceLocation, targetLocation) = findLocations(cfg, source, target)
    val newLocation: Option[File] =
      if (Some(sourceLocation) == targetLocation) None
      else Some(targetLocation.map(_.dir) getOrElse target)

    newLocation match {
      // location list must be updated only if target is not already a location
      case Some(newLoc) if targetLocation.isEmpty =>
        cfg.getLocationConf.remove(sourceLocation.dir).get
        cfg.getLocationConf.add(sourceLocation.copy(dir = newLoc)).get
      case _ =>
    }

    // update database before actual moving, because it relies on that
    // source still exists
    val sqlite = new SqliteBackend(cfg.getIndexDb)
    val n = sqlite.move(source, target, newLocation).get

    if (!indexOnly) {
      source.moveTo(target)
    }
    n
  }

}
