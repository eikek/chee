package chee.cli

import better.files.File
import chee.{Processing, Size}
import chee.conf._
import chee.cli.CryptOptions.{Opts => CryptOpts}
import chee.cli.LsOptions.{Opts => LsOpts}
import chee.cli.ProcessingOptions.{Opts => ProcOpts}
import chee.properties._
import com.typesafe.config.Config

object Thumb extends ScoptCommand with AbstractLs with TransparentDecrypt with ProcessingCommand {

  val name = "thumb"

  case class Opts(
    lsOpts: LsOptions.Opts = LsOptions.Opts(),
    cryptOpts: CryptOptions.Opts = CryptOptions.Opts(),
    procOpts: ProcessingOptions.Opts = ProcessingOptions.Opts(),
    size: Size = Size(100, 100)) extends ProcessingOpts {
    def updateCryptOpts(f: CryptOpts => CryptOpts) =
      copy(cryptOpts = f(cryptOpts))
    def updateLsOpts(f: LsOpts =>  LsOpts) =
      copy(lsOpts = f(lsOpts))
    def updateProcOpts(f: ProcOpts => ProcOpts) =
      copy(procOpts = f(procOpts))
  }

  type T = Opts

  val defaults = Opts()

  val parser = new ProcessingParser {
    addDefaultOptions()

    opt[Size]("size") valueName("<width>x<height>") action { (s, c) =>
      c.copy(size = s)
    } text ("The size to scale to. Default is 100x100.")

    addQuery(_ updateLsOpts _)
  }

  def exec(cfg: Config, opts: Opts): Unit = {
    val lsOpts = opts.lsOpts.appendQuery(cfg.getString("chee.queries.thumb-default"))
    exec(cfg, opts, findDecrypt(cfg, lsOpts, opts.cryptOpts))
  }

  def thumbAction(cfg: Config, procOpts: ProcOpts, size: Size): MapGet[Option[File]] =
    Processing.cover(size, makeOutFile(cfg, "cover", procOpts),
      cfg.getScaleMethod("chee.scalemethod.thumb"))

  def processingAction(cfg: Config, opts: Opts): MapGet[Boolean] =
    thumbAction(cfg, opts.procOpts, opts.size).flatMap(Processing.imageOverlay)

  def procOpts(opts: Opts) = opts.procOpts
}
