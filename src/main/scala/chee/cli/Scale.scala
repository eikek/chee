package chee.cli

import com.typesafe.config.Config
import chee.CheeConf.Implicits._
import chee.Size
import chee.properties._
import chee.Processing

object Scale extends ScoptCommand with AbstractLs with ProcessingCommand {

  val name = "scale"

  case class Opts(
    lsOpts: LsOptions.Opts = LsOptions.Opts(),
    procOpts: ProcessingOptions.Opts = ProcessingOptions.Opts(),
    factor: Option[Double] = None,
    maxLen: Option[Int] = None
  )

  type T = Opts

  val defaults = Opts()

  val parser = new Parser with LsOptions[Opts] with ProcessingOptions[Opts] {
    addLsOptions((c, f) => c.copy(lsOpts = f(c.lsOpts)))
    addProcessingOptions((c, f) => c.copy(procOpts = f(c.procOpts)))

    opt[Double]('m', "multiply") action { (m, c) =>
      c.copy(factor = Some(m))
    } text ("Scale by a factor of `m'.")

    opt[Int]('l', "maxlen") action { (n, c) =>
      c.copy(maxLen = Some(n))
    } text ("Scale such that the longest side of the image is not more than\n"+
      "        `maxlen'.")

    queryArg((c, f) => c.copy(lsOpts = f(c.lsOpts)))

    checkConfig { opts =>
      (opts.factor, opts.maxLen) match {
        case (Some(f), Some(n)) => failure("Either one of `maxlen' or `multiply' must be given.")
        case (None, None) => failure("Either one of `maxlen' or `multiply' must be given.")
        case _ => success
      }
    }
  }

  def exec(cfg: Config, opts: Opts): Unit = {
    exec(cfg, opts, find(cfg, opts.lsOpts))
  }

  def processingAction(cfg: Config, opts: Opts): MapGet[Boolean] =
    (opts.factor, opts.maxLen) match {
      case (Some(f), _) =>
        Processing.scaleByFactor(f, makeOutFile(cfg, "scale", opts.procOpts),
          cfg.getScaleMethod("chee.scalemethod.scale"))
      case (_, Some(n)) =>
        Processing.scaleMaxLen(n, makeOutFile(cfg, "scale", opts.procOpts),
          cfg.getScaleMethod("chee.scalemethod.scale"))
      case _ =>
        sys.error("Invalid input. Option parser should have handled this case")
    }


  def procOpts(opts: Opts) = opts.procOpts
}
