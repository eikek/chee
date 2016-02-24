package chee.cli

import com.typesafe.config.Config
import scala.sys.process.Process
import better.files._
import chee.properties._
import chee.properties.Patterns._
import chee.query._
import chee.properties.MapGet._
import chee.CheeConf.Implicits._

object MkTree extends AbstractLs {

  type T = Opts

  val name = "mktree"
  val defaults = Opts()

  sealed trait Action
  object Action {
    case object Symlink extends Action
    case object Copy extends Action
  }

  sealed trait Result
  object Result {
    case object Ok extends Result
    case object Overwritten extends Result
    case object Skipped extends Result
    case object FileNotFound extends Result
  }

  case class Opts(
    lsOpts: LsOpts = LsOpts(),
    overwrite: Boolean = false,
    action: Action = Action.Symlink,
    pattern: String = "",
    target: File = file"."
  ) extends CommandOpts

  val parser = new LsOptionParser {
    def copyLsOpts(o: Opts, lso: LsOpts) = o.copy(lsOpts = lso)
    def moreOptions(): Unit = {
      opt[Unit]('s', "symlink") action { (_, c) =>
        c.copy(action = Action.Symlink)
      } text ("Symlink files into the target directory (the default).")

      opt[Unit]('c', "copy") action { (_, c) =>
        c.copy(action = Action.Copy)
      } text ("Copy files into the target directory.")

      opt[Unit]("overwrite") action { (_, c) =>
        c.copy(overwrite = true)
      } text ("Whether to overwrite existing files.")

      opt[String]('p', "pattern") action { (p, c) =>
        c.copy(pattern = p)
      } text ("The pattern used to create the target path.")

      opt[File]("target") valueName("<directory>") action { (f, c) =>
        c.copy(target = f)
      } text ("The target directory. If not specified the current working\n" +
        "        directory is used.")
    }
  }

  val defaultPattern: Pattern = {
    import Patterns._
    seq(
      lookupAlt(Seq(Ident.created, Ident.lastModified), Some("yyyy")),
      raw(java.io.File.separator),
      lookupAlt(Seq(Ident.created, Ident.lastModified), Some("MM")),
      raw(java.io.File.separator),
      lookupAlt(Seq(Ident.created, Ident.lastModified), Some("dd-HH-mm")),
      raw("_"),
      lookup(Ident.filename))
  }

  override def exec(cfg: Config, opts: T): Unit = {
    if (opts.lsOpts.query.isEmpty) {
      chee.UserError("You must specify a query. If you really want to select all files, use a query like `len>0'.")
    } else {
      super.exec(cfg, opts)
    }
  }

  def makeAction(overwrite: Boolean, target: File, a: (File, File) => Unit): MapGet[Result] =
    existingPath.map {
      case Some(src) =>
        if (target.exists && !overwrite) Result.Skipped
        else {
          val result = if (target.exists) Result.Overwritten else Result.Ok
          target.parent.createDirectories()
          a(src, target)
          result
        }
      case _ => Result.FileNotFound
    }

  def symlink(source: File, target: File): Unit = {
    if (target.exists) target.delete()
    target.linkTo(source, true)
  }

  def copy(source: File, target: File): Unit = {
    source.copyTo(target, overwrite = true)
  }

  val logDone = Progress.seq[Result, Int](
    Progress.after { (count, result, _) =>
      outln(result.toString)
      count + 1
    },
    Progress.done { (count, dur) =>
      outln(s"${count} files processed in ${chee.Timing.format(dur)}")
    }
  )

  def makeProgress(action: String, target: MapGet[File]): Progress[Result, Int] = {
    val progress = Progress.before[Result, Int] {
      pair(value(Ident.path), target).map { case (p, t) =>
        out(s"$action ${p.get} → ${t.path} … ")
      }
    }
    progress andThen logDone
  }

  def exec(cfg: Config, opts: T, props: Stream[LazyMap]): Unit = {
    val filename =
      if (opts.pattern.isEmpty) defaultPattern
      else cfg.findCustomFormat(opts.pattern) match {
        case Right(p) => p
        case Left(msg) => chee.UserError(msg)
      }

    val target = filename.right(userError).map(n => opts.target / n)
    val action = target.flatMap(t => makeAction(opts.overwrite, t, opts.action match {
      case Action.Symlink => symlink
      case Action.Copy => copy
    }))
    makeProgress(opts.action.toString, target).foreach(0)(props, action)
  }
}
