package chee.cli

import chee.query.Progress
import chee.properties._
import chee.properties.MapGet._
import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging

trait CryptCommand { self: ScoptCommand with AbstractLs =>

  case class Opts(
    lsOpts: LsOptions.Opts = LsOptions.Opts(),
    cryptOpts: CryptOptions.Opts = CryptOptions.Opts(),
    parallel: Boolean = false
  )

  type T = Opts

  val defaults = Opts()

  def progress(verb: String, parallel: Boolean) = Progress.seq[Unit, Int](
    Progress.before(valueForce(Ident.filename).map { f =>
      if (! parallel) out(s"${verb}ing $f … ")
    }),
    Progress.after { n =>
      if (!parallel) outln("done")
      else out(".")
      n + 1
    },
    Progress.done { (n, dur) =>
      val msg = s"${verb}ed $n files in ${chee.Timing.format(dur)}"
      logger.trace(msg)
      if (parallel) out("\n")
      outln(msg)
    }
  )

  def runProcess(props: Stream[LazyMap], processor: MapGet[Boolean], parallel: Boolean): Unit = {
    val action = unit(())
    val prog = progress(name.capitalize, parallel)
    if (parallel) {
      prog.foreach(0)(MapGet.parfilter(props, processor), action)
    } else {
      prog.foreach(0)(MapGet.filter(props, processor), action)
    }
  }

  def processingAction(cfg: Config, opts: CryptOptions.Opts): MapGet[Boolean] 

  def exec(cfg: Config, opts: Opts): Unit = {
    val proc = processingAction(cfg, opts.cryptOpts)
    val props = find(cfg, opts.lsOpts)
    runProcess(props, proc, opts.parallel)
  }
}
