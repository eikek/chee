package chee.cli

import com.typesafe.config.Config
import chee.CheeConf.Implicits._
import chee.Size
import chee.properties._
import chee.Processing

object Thumb extends ScoptCommand with AbstractLs with TransparentDecrypt with ProcessingCommand {

  val name = "thumb"

  case class Opts(
    lsOpts: LsOptions.Opts = LsOptions.Opts(),
    cryptOpts: CryptOptions.Opts = CryptOptions.Opts(),
    procOpts: ProcessingOptions.Opts = ProcessingOptions.Opts(),
    size: Size = Size(100, 100)
  )

  type T = Opts

  val defaults = Opts()

  val parser = new Parser with LsOptions[Opts] with CryptOptions[Opts] with ProcessingOptions[Opts] {
    note("\nFind options:")
    addLsOptions((c, f) => c.copy(lsOpts = f(c.lsOpts)))
    note("\nDecrypt options:")
    enable((c, f) => c.copy(cryptOpts = f(c.cryptOpts)))
    addDecryptOptions((c, f) => c.copy(cryptOpts = f(c.cryptOpts)))
    note("\nProcessing options:")
    addProcessingOptions((c, f) => c.copy(procOpts = f(c.procOpts)))

    opt[Size]("size") valueName("<width>x<height>") action { (s, c) =>
      c.copy(size = s)
    } text ("The size to scale to. Default is 100x100.")

    note("")
    queryArg((c, f) => c.copy(lsOpts = f(c.lsOpts)))
  }

  def exec(cfg: Config, opts: Opts): Unit = {
    exec(cfg, opts, findDecrypt(cfg, opts.lsOpts, opts.cryptOpts))
  }

  def processingAction(cfg: Config, opts: Opts): MapGet[Boolean] =
    Processing.cover(opts.size, makeOutFile(cfg, "cover", opts.procOpts),
      cfg.getScaleMethod("chee.scalemethod.thumb"))

  def procOpts(opts: Opts) = opts.procOpts
}
