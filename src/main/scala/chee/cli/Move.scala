package chee.cli

import chee.CheeApi._
import Move.{Opts, SrcDest}
import better.files._
import chee.properties._
import MapGet._
import chee.util.files._
import com.typesafe.config.Config

class Move extends ScoptCommand with LockSupport with ProgressDef {

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
      case SrcDest(src, trg) if src.exists(s => trg.childOf(s)) =>
        failure("Cannot move a file into a sub directory of itself: "+
          src.filter(s => trg.childOf(s)).mkString(", "))
      case _ =>
        success
    }
  }

  def exec(cfg: Config, opts: Move.Opts): Unit = withLock(cfg) {
    val progress = resultCountProgress {
      pair(valueForce(Ident("source")), valueForce(Ident("target"))).map { case (src, trg) =>
        out(s"Move $src → $trg … ")
      }
    }
    val apiMove = CheeApi(cfg).moveFiles(ResultCount.empty, progress)_
    val param = MoveParam(opts.files.init, opts.files.last, opts.indexOnly)
    apiMove(param)
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
}
