package chee.cli

import com.typesafe.config.Config
import better.files._
import chee.properties._
import chee.query._
import chee.conf._
import com.typesafe.scalalogging.LazyLogging
import LocationSync.Opts

class LocationSync extends ScoptCommand with LockSupport {
  import java.time.Duration
  import chee.LocationConf
  import chee.LocationConf.Entry
  import MapGet._
  import Predicates._

  type T = Opts

  val name = "sync"
  val defaults = Opts()

  val parser = new Parser {
    opt[Unit]("reindex") optional() action { (_, c) =>
      c.copy(reindex = true)
    } text ("Drop the index and read in registered locations again.")

    opt[Unit]("all") optional() action { (_, c) =>
      c.copy(all = true)
    } text ("Synchronise all locations.")

    arg[Seq[File]]("<locations>") optional() unbounded() action { (x, c) =>
      c.copy(dirs = c.dirs ++ x)
    } text ("The list of locations to synchronise. Can be empty when\n" +
      "        --all is specified.")

    checkConfig { opts =>
      if (opts.dirs.isEmpty && !opts.all) failure("Use --all to sync all known locations.")
      else if (opts.dirs.nonEmpty && opts.all) failure("Do not use --all when also specifying locations.")
      else success
    }
  }

  def checkDirsToSync(locConf: LocationConf, opts: Opts): Seq[LocationConf.Entry] = {
    val locs = locConf.list.get
    val dirsToSync =
      if (opts.dirs.isEmpty) locs.map(_.dir)
      else opts.dirs

    Location.checkRegisteredLocations(locConf, dirsToSync)
    dirsToSync.find(!_.exists) foreach { e =>
      chee.UserError(s"Location `${e.path}' does not exist.\nUse the `${LocationDelete.name}' " +
        "command to remove a location.")
    }

    if (opts.dirs.isEmpty) locs
    else opts.dirs.map(f => locs.find(_.dir == f).get)
  }

  def reindex(locEntry: Entry, cfg: Config, sqlite: SqliteBackend): Unit = {
    val count = sqlite.delete(Prop(Comp.Eq, Ident.location -> locEntry.dir.path.toString)).get
    outln(s"Removed `${locEntry.dir.path}' from index: $count files removed")
    val addOpts = LocationAdd.Opts().copy(
      dirs = Seq(locEntry.dir),
      recursive = locEntry.recursive,
      all = locEntry.all,
      query = locEntry.query)
    new LocationAdd().indexDirs(cfg, addOpts, sqlite)
  }

  sealed trait Result
  object Result {
    case object Delete extends Result
    case object Add extends Result
    case object Update extends Result
    case object NoAction extends Result
  }

  def whenPresent[A](a: MapGet[Option[A]])(f: A => MapGet[Result]): MapGet[Result] =
    a.flatMap(x => if (x.isDefined) f(x.get) else unit(Result.NoAction))

  def whenPresent[A](a: MapGet[(Option[A], Option[A])])(f: (A, A) => MapGet[Result]): MapGet[Result] =
    a.flatMap {
      case (Some(a1), Some(a2)) =>
        f(a1, a2)
      case _ =>
        unit(Result.NoAction)
    }

  def addToIndex(sqlite: SqliteBackend): MapGet[Result] = MapGet(lm => {
    val (next, b) = sqlite.insertOne(lm).get
    logger.debug(s"Added ${value(Ident.path).result(next).get} to index")
    (next, if (b) Result.Add else Result.NoAction)
  })

  def updateIndex(sqlite: SqliteBackend): MapGet[Result] =
    whenPresent(pair(value(Ident.path), value(Ident.checksum))) { (path, checksum) =>
      if (sqlite.checksumMatch(path, checksum).get) unit(Result.NoAction)
      else get.map(sqlite.updateOne(_).get).flatMap {
        case (next, b) =>
          logger.debug(s"Update index for ${value(Ident.path).result(next).get}")
          set(next).map(_ => if (b) Result.Update else Result.NoAction)
      }
    }

  def deleteIndex(sqlite: SqliteBackend): MapGet[Result] =
    whenPresent(find(Ident.path)) { p =>
      val n = sqlite.delete(Prop(Comp.Eq, p)).get
      logger.debug(s"Deleted index for ${p}")
      unit(if (n>0) Result.Delete else Result.NoAction)
    }

  def fsSync(sqlite: SqliteBackend): MapGet[Result] =
    whenPresent(find(Ident.path))(_ match {
      case Property(id, path) if File(path).exists =>
        if (!sqlite.pathExists(path).get) addToIndex(sqlite)
        else updateIndex(sqlite)
      case prop =>
        deleteIndex(sqlite)
    })


  def fileCondition(opts: Entry, cfg: Config): Condition = {
    val query = cfg.makeQuery
    AbstractLs.getFileCondition(query, opts.query, cfg, opts.all) match {
      case Right(cond) => cond
      case Left(msg) => chee.UserError(msg)
    }
  }

  case class Data(
    added: Int = 0,
    updated: Int = 0,
    deleted: Int = 0,
    none: Int = 0) {
    def incAdded = copy(added = added + 1)
    def incUpdated = copy(updated = updated + 1)
    def incDeleted = copy(deleted = deleted + 1)
    def incNone = copy(none = none + 1)
  }

  val syncProgress: Progress[Result, Data] =
    Progress.after((data, result, _) =>
      result match {
        case Result.Add =>
          out("+")
          data.incAdded
        case Result.Update =>
          out("u")
          data.incUpdated
        case Result.Delete =>
          out("-")
          data.incDeleted
        case Result.NoAction =>
          out(".")
          data.incNone
      })

  val logDone = Progress.done[Result, Data] { (data, dur) =>
    out(s"\nAdded ${data.added}, ")
    out(s"updated ${data.updated}, ")
    out(s"deleted ${data.deleted} and ")
    outln(s"skipped ${data.none} in ${chee.Timing.format(dur)}")
  }

  def sync(cfg: Config, opts: Opts): Unit = {
    val sqlite = new SqliteBackend(cfg.getIndexDb)
    val entries = checkDirsToSync(cfg.getLocationConf, opts)
    if (opts.reindex) {
      entries.foreach(reindex(_, cfg, sqlite))
    } else {
      for (locEntry <- entries) {
        outln(s"Sync ${locEntry.dir.path} …")
        val files = FileBackend.find(fileCondition(locEntry, cfg), locEntry.dir, locEntry.recursive)
        val (data, dur) = syncProgress.foreach(Data())(files, fsSync(sqlite))
        // delete non-existing files from db
        outln("\nDeleting non existing files from index …")
        val dbfiles = MapGet.filter(sqlite.find(Prop(Comp.Eq, Ident.location -> locEntry.dir.path.toString)).get, not(fileExists))
        (syncProgress andThen logDone).foreach(data, dur)(dbfiles, deleteIndex(sqlite))
      }
    }
  }

  def exec(cfg: Config, opts: Opts): Unit = withLock(cfg) {
    sync(cfg, opts)
  }
}

object LocationSync {
  case class Opts(
    reindex: Boolean = false,
    all: Boolean = false,
    dirs: Seq[File] = Seq.empty)
}
