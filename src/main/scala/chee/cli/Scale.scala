package chee.cli

import com.typesafe.config.Config
import chee.CheeConf.Implicits._
import chee.Size
import chee.properties._
import chee.Processing

object Scale extends ProcessingCommand {

  val name = "scale"

  case class Opts(
    lsOpts: LsOpts = LsOpts(),
    procOpts: ProcOpts = ProcOpts(),
    factor: Option[Double] = None,
    maxLen: Option[Int] = None
  ) extends ProcessingOpts

  type T = Opts

  val defaults = Opts()

  val parser = new ProcessingOptionParser {
    def copyLsOpts(o: Opts, lso: LsOpts) = o.copy(lsOpts = lso)
    def copyProcOpts(o: Opts, po: ProcOpts) = o.copy(procOpts = po)
    override def moreOptions(): Unit = {
      super.moreOptions()

      opt[Double]('m', "multiply") action { (m, c) =>
        c.copy(factor = Some(m))
      } text ("Scale by a factor of `m'.")

      opt[Int]('l', "maxlen") action { (n, c) =>
        c.copy(maxLen = Some(n))
      } textW ("Scale such that the longest side of the image is not more than `maxlen'.")

      checkConfig { opts =>
        (opts.factor, opts.maxLen) match {
          case (Some(f), Some(n)) => failure("Either one of `maxlen' or `multiply' must be given.")
          case (None, None) => failure("Either one of `maxlen' or `multiply' must be given.")
          case _ => success
        }
      }
    }
  }

  def processingAction(cfg: Config, opts: Opts): MapGet[Boolean] =
    (opts.factor, opts.maxLen) match {
      case (Some(f), _) =>
        Processing.scaleByFactor(f, makeOutFile(cfg, "scale", opts),
          cfg.getScaleMethod("chee.scalemethod.scale"))
      case (_, Some(n)) =>
        Processing.scaleMaxLen(n, makeOutFile(cfg, "scale", opts),
          cfg.getScaleMethod("chee.scalemethod.scale"))
      case _ =>
        sys.error("Invalid input. Option parser should have handled this case")
    }
}
