package chee.cli

import com.typesafe.config.Config
import chee.CheeConf.Implicits._
import chee.Size
import chee.properties._
import chee.Processing

object Thumb extends ScoptCommand with AbstractLs with ProcessingCommand {

  val name = "thumb"

  case class Opts(
    lsOpts: LsOptions.Opts = LsOptions.Opts(),
    procOpts: ProcessingOptions.Opts = ProcessingOptions.Opts(),
    size: Size = Size(100, 100)
  )

  type T = Opts

  val defaults = Opts()

  val parser = new Parser with LsOptions[Opts] with ProcessingOptions[Opts] {
    addLsOptions((c, f) => c.copy(lsOpts = f(c.lsOpts)))
    addProcessingOptions((c, f) => c.copy(procOpts = f(c.procOpts)))

    opt[Size]("size") valueName("<width>x<height>") action { (s, c) =>
      c.copy(size = s)
    } text ("The size to scale to. Default is 100x100.")

    queryArg((c, f) => c.copy(lsOpts = f(c.lsOpts)))
  }

  def exec(cfg: Config, opts: Opts): Unit = {
    exec(cfg, opts, find(cfg, opts.lsOpts))
  }

  def processingAction(cfg: Config, opts: Opts): MapGet[Boolean] =
    Processing.cover(opts.size, makeOutFile(cfg, "cover", opts.procOpts),
      cfg.getScaleMethod("chee.scalemethod.thumb"))

  def procOpts(opts: Opts) = opts.procOpts
}
