package chee.cli

import com.typesafe.config.Config
import chee.CheeConf.Implicits._
import chee.Size
import chee.properties._
import chee.Processing

object Thumb extends ProcessingCommand {

  val name = "thumb"

  case class Opts(
    lsOpts: LsOpts = LsOpts(),
    procOpts: ProcOpts = ProcOpts(),
    size: Size = Size(100, 100)
  ) extends ProcessingOpts

  type T = Opts

  val defaults = Opts()

  val parser = new ProcessingOptionParser {
    def copyLsOpts(o: Opts, lso: LsOpts) = o.copy(lsOpts = lso)
    def copyProcOpts(o: Opts, po: ProcOpts) = o.copy(procOpts = po)
    override def moreOptions(): Unit = {
      super.moreOptions()

      opt[Size]("size") valueName("<width>x<height>") action { (s, c) =>
        c.copy(size = s)
      } text ("The size to scale to. Default is 100x100.")
    }
  }

  def processingAction(cfg: Config, opts: Opts): MapGet[Boolean] =
    Processing.cover(opts.size, makeOutFile(cfg, "cover", opts),
      cfg.getScaleMethod("chee.scalemethod.thumb"))

}
