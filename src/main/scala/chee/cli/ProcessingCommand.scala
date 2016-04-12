package chee.cli

import better.files._
import chee.CheeConf.Implicits._
import chee.Processing
import chee.cli.CryptOptions.{Opts => CryptOpts}
import chee.cli.LsOptions.{Opts => LsOpts}
import chee.cli.ProcessingOptions.{Opts => ProcOpts}
import chee.properties._
import chee.properties.Patterns._
import chee.query._
import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging

trait ProcessingCommand extends ScoptCommand {

  trait ProcessingOpts {
    def updateCryptOpts(f: CryptOpts => CryptOpts): T
    def updateLsOpts(f: LsOpts =>  LsOpts): T
    def updateProcOpts(f: ProcOpts => ProcOpts): T
  }

  type T <: ProcessingOpts

  abstract class ProcessingParser extends Parser with LsOptions[T] with CryptOptions[T] with ProcessingOptions[T] {
    def addDefaultOptions(): Unit = {
      addLsOptions(_ updateLsOpts _)
      addDecryptOptions(_ updateCryptOpts _, enableOpt = true)
      addProcessingOptions(_ updateProcOpts _)
    }
  }

  val progress = Progress.seq[Unit, Int](
    Progress.after { n => n + 1},
    Progress.done { (n, dur) =>
      logger.trace(s"Scaled $n images in ${chee.Timing.format(dur)}")
    }
  )

  def makeOutFile(cfg: Config, prefix: String, opts: ProcessingOptions.Opts): MapGet[File] = {
    import chee.properties.Patterns._
    val dir = opts.outdir getOrElse cfg.getFile("chee.scaleddir")
    val format = opts.nameformat match {
      case Some(_) => cfg.getFormat(opts.nameformat, "") match {
        case Right(f) => f
        case Left(m) => chee.UserError(m)
      }
      case _ => seq(
        maxlen(25, lookup(Ident.checksum)),
        raw(s"-${prefix}-"),
        lookup(Processing.targetWidth), raw("x"), lookup(Processing.targetHeight),
        raw(".jpg")
      )
    }
    format.right(userError).map(name => dir / name)
  }

  def getFormat(cfg: Config, pattern: Option[String]): Either[String, Pattern] =
    pattern match {
      case None => Right(FormatPatterns.onelineNoLocation)
      case Some("oneline") => Right(FormatPatterns.onelineNoLocation)
      case _ => cfg.getFormat(pattern, "")
    }

  def processingAction(cfg: Config, opts: T): MapGet[Boolean]

  def procOpts(opts: T): ProcessingOptions.Opts

  def exec(cfg: Config, opts: T, props: Stream[LazyMap]): Unit = {
    getFormat(cfg, procOpts(opts).pattern) match {
      case Right(pattern) =>
        val proc = processingAction(cfg, opts)
        val action = MapGet.get.map(m => out(pattern.right(userError).result(m)))
        if (procOpts(opts).parallel) {
          progress.foreach(0)(MapGet.parfilter(props, proc), action)
        } else {
          progress.foreach(0)(MapGet.filter(props, proc), action)
        }
      case Left(msg) => chee.UserError(msg)
    }
  }
}
