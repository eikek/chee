package chee.cli

import com.typesafe.config.Config
import scala.sys.process.Process
import chee.properties._
import chee.properties.Patterns._

object View extends ScoptCommand with AbstractLs {

  type T = LsOptions.Opts

  val name = "view"
  val defaults = LsOptions.Opts()

  val parser = new Parser with LsOptions[LsOptions.Opts] {
    addLsOptions((e, f) => f(e))
    queryArg((e, f) => f(e))
  }

  def exec(cfg: Config, opts: LsOptions.Opts): Unit = {
    val props = find(cfg, opts)
    val list = props.map(Patterns.lookup(Ident.path).right(userError).result)
    if (list.isEmpty) outln("No files selected")
    else runViewer(cfg, list.toSeq)
  }


  def runViewer(cfg: Config, files: Seq[String]): Unit = {
    val cmd = cfg.getString("chee.programs.viewer").split("\\s+").toSeq ++ files
    Process(cmd.head, cmd.drop(1)).!
  }
}
