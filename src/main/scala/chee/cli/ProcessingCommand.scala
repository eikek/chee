package chee.cli

import better.files._
import com.typesafe.config.Config
import chee.Size
import chee.properties._
import chee.properties.Patterns._
import chee.query._
import chee.Processing
import chee.CheeConf.Implicits._

abstract class ProcessingCommand extends AbstractLs {

  case class ProcOpts(
    pattern: Option[String] = None,
    parallel: Boolean = false,
    outdir: Option[File] = None,
    nameformat: Option[String] = None
  )

  trait ProcessingOpts extends CommandOpts {
    def procOpts: ProcOpts
  }

  type T <: ProcessingOpts

  abstract class ProcessingOptionParser extends LsOptionParser {
    def copyProcOpts(o: T, po: ProcOpts): T
    def copyLsOpts(o: T, lso: LsOpts): T

    def moreOptions(): Unit = {
      opt[Unit]('c', "concurrent") action { (_, c) =>
        copyProcOpts(c, c.procOpts.copy(parallel = true))
      } text("Process files concurrently.")

      opt[String]('p', "pattern") action { (p, c) =>
        copyProcOpts(c, c.procOpts.copy(pattern = Some(p)))
      } text ("The format pattern used to print the result to stdout.")

      opt[File]('o', "outdir") action { (d, c) =>
        copyProcOpts(c, c.procOpts.copy(outdir = Some(d)))
      } text ("The directory to place generated images.") validate { f =>
        if (f.exists && !f.isDirectory) failure(s"${f.path} is an existing file")
        else success
      }

      opt[String]("nameformat") action { (f, c) =>
        copyProcOpts(c, c.procOpts.copy(nameformat = Some(f)))
      } text ("The format pattern used to create the target file name. It is\n"+
        "        evaluated with the properties of the original file with\n"+
        "        `width' and `height' replaced by the desired target values.")
    }
  }

  val progress = Progress.seq[Unit, Int](
    Progress.after { n => n + 1},
    Progress.done { (n, dur) =>
      logger.trace(s"Scaled $n images in ${chee.Timing.format(dur)}")
    }
  )

  def makeOutFile(cfg: Config, prefix: String, opts: T): MapGet[File] = {
    import chee.properties.Patterns._
    val dir = opts.procOpts.outdir getOrElse cfg.getFile("chee.scaleddir")
    val format = opts.procOpts.nameformat match {
      case Some(_) => cfg.getFormat(opts.procOpts.nameformat, "") match {
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

  def getFormat(cfg: Config, opts: ProcOpts): Either[String, Pattern] =
    opts.pattern match {
      case None => Right(FormatPatterns.onelineNoLocation)
      case Some("oneline") => Right(FormatPatterns.onelineNoLocation)
      case _ => cfg.getFormat(opts.pattern, "")
    }

  def processingAction(cfg: Config, opts: T): MapGet[Boolean]

  def exec(cfg: Config, opts: T, props: Stream[LazyMap]): Unit = {
    getFormat(cfg, opts.procOpts) match {
      case Right(pattern) =>
        val proc = processingAction(cfg, opts)
        val action = MapGet.get.map(m => out(pattern.right(userError).result(m)))
        if (opts.procOpts.parallel) {
          progress.foreach(0)(MapGet.parfilter(props, proc), action)
        } else {
          progress.foreach(0)(MapGet.filter(props, proc), action)
        }
      case Left(msg) => chee.UserError(msg)
    }
  }
}
