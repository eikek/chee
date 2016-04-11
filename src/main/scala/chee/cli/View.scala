package chee.cli

import com.typesafe.config.Config
import scala.sys.process.Process
import chee.properties._
import chee.properties.Patterns._

object View extends ScoptCommand with AbstractLs with TransparentDecrypt {

  case class Opts(
    lsOpts: LsOptions.Opts = LsOptions.Opts(),
    cryptOpts: CryptOptions.Opts = CryptOptions.Opts()
  )

  type T = Opts

  val name = "view"
  val defaults = Opts()

  val parser = new Parser with LsOptions[Opts] with CryptOptions[Opts] {
    note("\nFind options:")
    addLsOptions((c, f) => c.copy(lsOpts = f(c.lsOpts)))
    note("\nDecrypt options:")
    enable((c, f) => c.copy(cryptOpts = f(c.cryptOpts)))
    addDecryptOptions((c, f) => c.copy(cryptOpts = f(c.cryptOpts)))
    note("")
    queryArg((c, f) => c.copy(lsOpts = f(c.lsOpts)))
  }

  def exec(cfg: Config, opts: Opts): Unit = {
    val props = findDecrypt(cfg, opts.lsOpts, opts.cryptOpts)
    val list = props.map(Patterns.lookup(Ident.path).right(userError).result)
    if (list.isEmpty) outln("No files selected")
    else runViewer(cfg, list.toSeq)
  }


  def runViewer(cfg: Config, files: Seq[String]): Unit = {
    val cmd = cfg.getString("chee.programs.viewer").split("\\s+").toSeq ++ files
    Process(cmd.head, cmd.drop(1)).!
  }
}
