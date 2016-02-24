package chee.cli

import com.typesafe.config.Config
import scala.sys.process.Process
import chee.properties._
import chee.properties.Patterns._

object View extends AbstractLs {

  type T = Opts

  val name = "view"
  val defaults = Opts()

  case class Opts(lsOpts: LsOpts = LsOpts()) extends CommandOpts

  val parser = new LsOptionParser {
    def copyLsOpts(o: Opts, lso: LsOpts) = o.copy(lsOpts = lso)
    def moreOptions(): Unit = ()
  }

  def exec(cfg: Config, opts: T, props: Stream[LazyMap]): Unit = {
    val list = props.map(Patterns.lookup(Ident.path).right(userError).result)
    if (list.isEmpty) outln("No files selected")
    else runViewer(cfg, list.toSeq)
  }

  def runViewer(cfg: Config, files: Seq[String]): Unit = {
    val cmd = cfg.getString("chee.programs.viewer").split("\\s+").toSeq ++ files
    Process(cmd.head, cmd.drop(1)).!
  }
}
