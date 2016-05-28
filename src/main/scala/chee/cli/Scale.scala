package chee.cli

import better.files.File
import chee.conf._
import chee.Processing
import chee.cli.CryptOptions.{Opts => CryptOpts}
import chee.cli.LsOptions.{Opts => LsOpts}
import chee.cli.ProcessingOptions.{Opts => ProcOpts}
import chee.properties._
import com.typesafe.config.Config

object Scale extends ScoptCommand with AbstractLs with TransparentDecrypt with ProcessingCommand {

  val name = "scale"

  case class Opts(
    lsOpts: LsOpts = LsOpts(),
    cryptOpts: CryptOpts = CryptOpts(),
    procOpts: ProcOpts = ProcOpts(),
    factor: Option[Double] = None,
    maxLen: Option[Int] = None
  ) extends ProcessingOpts {
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

    opt[Double]('m', "multiply") action { (m, c) =>
      c.copy(factor = Some(m))
    } text ("Scale by a factor of `m'.")

    opt[Int]('l', "maxlen") action { (n, c) =>
      c.copy(maxLen = Some(n))
    } textW ("Scale such that the longest side of the image is not more than `maxlen'.")

    addQuery(_ updateLsOpts _)

    checkConfig { opts =>
      (opts.factor, opts.maxLen) match {
        case (Some(f), Some(n)) => failure("Either one of `maxlen' or `multiply' must be given.")
        case (None, None) => failure("Either one of `maxlen' or `multiply' must be given.")
        case _ => success
      }
    }
  }

  def exec(cfg: Config, opts: Opts): Unit = {
    val lsOpts = opts.lsOpts.appendQuery(cfg.getString("chee.queries.scale-default"))
    exec(cfg, opts, findDecrypt(cfg, lsOpts, opts.cryptOpts))
  }

  def scaleAction(cfg: Config, procOpts: ProcOpts, factor: Option[Double], maxLen: Option[Int]): MapGet[Option[File]] =
    (factor, maxLen) match {
      case (Some(f), _) =>
        Processing.scaleByFactor(f, makeOutFile(cfg, "scale", procOpts),
          cfg.getScaleMethod("chee.scalemethod.scale"))
      case (_, Some(n)) =>
        Processing.scaleMaxLen(n, makeOutFile(cfg, "scale", procOpts),
          cfg.getScaleMethod("chee.scalemethod.scale"))
      case _ =>
        sys.error("Invalid input. Option parser should have handled this case")
    }

  def processingAction(cfg: Config, opts: Opts): MapGet[Boolean] =
    scaleAction(cfg, opts.procOpts, opts.factor, opts.maxLen).flatMap(Processing.imageOverlay)

  def procOpts(opts: Opts) = opts.procOpts
}
