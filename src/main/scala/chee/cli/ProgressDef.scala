package chee.cli

import chee.query.Progress
import chee.Timing
import chee.FileOps.Result
import chee.properties.{Ident, MapGet}

trait ProgressDef {
  self: Command =>

  def resultCountProgress: Progress[Result, ResultCount] =
    resultCountProgress {
      MapGet.valueForce(Ident.path).map { path =>
        out(s"${name.capitalize} $path … ")
      }
    }

  def resultCountProgress(before: MapGet[Unit]): Progress[Result, ResultCount] =
    Progress.seq[Result, ResultCount](
      Progress.before(before),
      Progress.after { (data, result, dur) =>
        outln(result.toString)
        data.inc(result)
      },
      Progress.done { (data, dur) =>
        val report = data.counter.foldLeft(""){ case (s, (result, count)) =>
          s + s"$result: ${data.get(result)} files; "
        }
        outln(s"${report}in ${Timing.format(dur)}")
      }
    )


}
