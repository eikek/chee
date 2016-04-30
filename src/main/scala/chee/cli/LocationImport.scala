package chee.cli

import scala.util.{Try, Success, Failure}
import com.typesafe.config.Config
import better.files._
import chee.properties._
import chee.properties.MapGet._
import chee.query._
import chee.conf._
import chee.util.files._
import chee.cli.LsOptions.{ Opts => LsOpts }
import LocationImport.Opts

class LocationImport extends ScoptCommand with AbstractLs with LockSupport {

  type T = Opts

  val defaults = Opts()

  val name = "import"

  val parser = new Parser with LsOptions[Opts] {
    private val lsAction: (Opts, LsOpts => LsOpts) => Opts =
      (c, f) => c.copy(lsOpts = f(c.lsOpts))

    def addSourceOptions(): Unit = {
      noteW("\nFind options:")
      recursive(lsAction)
      all(lsAction)
      skip(lsAction)
      first(lsAction)
      opt[String]('q', "query") action { (q, c) =>
        lsAction(c, _.copy(query = q))
      } text ("The query to apply to the source directory.")
    }

    addSourceOptions()

    noteW("\nImport options:")

    opt[Unit]("duplicates") action { (_, c) =>
      c.copy(duplicates = true)
    } text ("Import files that already exists in some location.")

    arg[File]("<sourcedir>") required() action { (f, c) =>
      lsAction(c, _.copy(directory = Some(f)))
    } text("The directory to import.")

    arg[File]("<location>") required() action { (f, c) =>
      c.copy(location = f)
    } text("The location to import into.")
  }

  sealed trait Result
  object Result {
    case object FileNotFound extends Result
    case object Duplicate extends Result
    case object Skipped extends Result
    case object Ok extends Result
  }

  def exists(sqlite: SqliteBackend): MapGet[Boolean] =
    sqlite.idExists.map(_.get)

  def setPath(path: File): MapGet[Unit] = modify { m =>
    m.add(Ident.path -> path.path.toString)
      .add(Ident.filename -> path.name)
  }

  def getSource(opts: Opts): File =
    opts.lsOpts.directory.getOrElse(sys.error("source dir is required"))

  def makeTarget(src: File, opts: Opts): File = {
    @annotation.tailrec
    def asNonExistent(f: File, n: Int = 1, max: Int = 500): File = f match {
      case _ if n >= max =>
        userError(s"Cannot find non-existing target to copy `${src.path}'. Tried to rename $max times.")
      case _ if f.exists =>
        asNonExistent(f.mapBaseName(_ +"-"+ n), n+1)
      case _ => f
    }
    val sub = getSource(opts) relativize src
    asNonExistent(opts.location / sub.toString)
  }

  def insertIndex(sqlite: SqliteBackend): MapGet[Result] = MapGet { lm =>
    val (next, _) = sqlite.insertOne(lm).get
    (next, Result.Ok)
  }

  def copy(opts: Opts, sqlite: SqliteBackend): MapGet[Result] =
    existingPath.flatMap {
      case Some(src) =>
        val target = makeTarget(src, opts)
        if (target.exists) unit(Result.Skipped)
        else {
          target.parent.createDirectories()
          src.copyTo(target)
          setPath(target).flatMap(_ => insertIndex(sqlite))
        }
      case None => unit(Result.FileNotFound)
    }

  val progress = Progress.seq[Result, Int](
    Progress.before(MapGet.path.map { p =>
      out(s"Import ${p.path} … ")
    }),
    Progress.after { (count, result, _) =>
      outln(result.toString)
      count + 1
    },
    Progress.done { (count, dur) =>
      outln(s"${count} files processed in ${chee.Timing.format(dur)}")
    }
  )

  def exec(cfg: Config, opts: Opts): Unit = withLock(cfg) {
    Location.checkRegisteredLocations(cfg.getLocationConf, Seq(opts.location))
    Location.checkNotRegisteredLocations(cfg.getLocationConf, Seq(getSource(opts)))
    val sqlite = new SqliteBackend(cfg.getIndexDb)

    val action = exists(sqlite).flatMap { ex =>
      if (ex && !opts.duplicates) unit(Result.Duplicate)
      else copy(opts, sqlite)
    }

    val added = Extraction.added(DateTime.now)
    val location: Property = (Ident.location -> opts.location.path.toString)
    val files = find(cfg, opts.lsOpts).map(_ + added + location)
    progress.foreach(0)(files, action)
  }
}

object LocationImport {
  case class Opts(
    lsOpts: LsOpts = LsOpts(),
    duplicates: Boolean = false,
    location: File = file".")
}
