package chee.cli

import chee.cli.CryptOptions.{Opts => CryptOpts}
import chee.cli.LsOptions.{Opts => LsOpts}
import chee.OS
import chee.properties._
import chee.properties.Patterns._
import com.typesafe.config.Config

object View extends ScoptCommand with AbstractLs with TransparentDecrypt {

  case class Opts(
    lsOpts: LsOpts = LsOpts(),
    cryptOpts: CryptOpts = CryptOpts()) {
    def updateCryptOpts(f: CryptOpts => CryptOpts) =
      copy(cryptOpts = f(cryptOpts))
    def updateLsOpts(f: LsOpts =>  LsOpts) =
      copy(lsOpts = f(lsOpts))
  }

  type T = Opts

  val name = "view"
  val defaults = Opts()

  val parser = new Parser with LsOptions[Opts] with CryptOptions[Opts] {
    addLsOptions(_ updateLsOpts _)
    addDecryptOptions(_ updateCryptOpts _, enableOpt = true)
    addQuery(_ updateLsOpts _)
  }

  def exec(cfg: Config, opts: Opts): Unit = {
    val props = findDecrypt(cfg, opts.lsOpts, opts.cryptOpts)
    val list = props.map(Patterns.lookup(Ident.path).right(userError).result)
    if (list.isEmpty) outln("No files selected")
    else runViewer(cfg, list.toSeq)
  }


  def runViewer(cfg: Config, files: Seq[String]): Unit = {
    OS.Command(cfg.getString("chee.programs.viewer"), files).flatMap(OS.Run.exec)
  }
}
