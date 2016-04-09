package chee.cli

import chee.query.Progress
import chee.properties._
import chee.properties.MapGet._
import com.typesafe.scalalogging.LazyLogging

trait CryptProgress {
  self: Command with LazyLogging =>

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
}
