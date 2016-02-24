package chee.cli

import scala.util.{Try, Success, Failure}
import com.typesafe.config.Config
import better.files._
import chee.properties._
import chee.properties.MapGet._
import chee.query._
import chee.CheeConf.Implicits._

object LocationImport extends ScoptCommand with LockSupport {

  case class Opts(
    recursive: Boolean = false,
    all: Boolean = false,
    first: Option[Int] = None,
    duplicates: Boolean = false,
    query: String = "",
    source: File = file".",
    location: File = file"."
  )

  type T = Opts

  val defaults = Opts()

  val name = "import"

  val parser = new CheeOptionParser[Opts](name) {
    opt[Unit]('r', "recursive") optional() action { (_, c) =>
      c.copy(recursive = true)
    } text ("Find files recursively.")

    opt[Unit]('a', "all") optional() action { (_, c) =>
      c.copy(all = true)
    } text ("Ignore the default query.")

    opt[Int]("first") valueName("<n>") optional() action { (n, c) =>
      c.copy(first = Some(n))
    } text ("Limit results to the first n items.")

    opt[String]('q', "query") action { (q, c) =>
      c.copy(query = q)
    } text ("The query to apply to the source directory.")

    opt[Unit]("duplicates") action { (_, c) =>
      c.copy(duplicates = true)
    } text ("Import files that already exists (in any location).")

    arg[File]("<sourcedir>") required() action { (f, c) =>
      c.copy(source = f)
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

  def makeTarget(src: File, opts: Opts): File = {
    @annotation.tailrec
    def asNonExistent(f: File, n: Int = 1, max: Int = 500): File =
      if (n >= max) chee.UserError(s"Cannot find non-existing target to copy `${src.path}'. Tried to rename $max times.")
      else if (f.exists) asNonExistent(f.mapBaseName(_ +"-"+ n), n+1)
      else f
    val sub = opts.source relativize src
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
    Location.checkNotRegisteredLocations(cfg.getLocationConf, Seq(opts.source))
    val sqlite = new SqliteBackend(cfg.getIndexDb)

    val action = exists(sqlite).flatMap { ex =>
      if (ex && !opts.duplicates) unit(Result.Duplicate)
      else copy(opts, sqlite)
    }

    val query = cfg.makeQuery
    AbstractLs.getFileCondition(query, opts.query, cfg, opts.all) match {
      case Right(cond) =>
        val added = Extraction.added(DateTime.now)
        val files = FileBackend.find(cond, opts.source, opts.recursive)
          .map(_ +  added + (Ident.location -> opts.location.path.toString))
        progress.foreach(0)(files, action)
      case Left(msg) => chee.UserError(msg)
    }
  }

}
