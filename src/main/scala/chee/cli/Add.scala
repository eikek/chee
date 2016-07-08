package chee.cli

import better.files.File
import Add._
import CryptOptions.{Opts => CryptOpts}
import chee.CheeApi._
import com.typesafe.config.Config

class Add extends ScoptCommand with ProgressDef with LockSupport {

  type T = Opts
  val defaults = Opts()

  val name = "add"

  val parser = new Parser with CryptOptions[Opts] with LsOptions[Opts] {
    addFileSettings(_ updateF _)
    addDecryptOptions(_ update _, enableOpt = true)

    noteW("\nAdd options:")
    opt[Unit]("fail-fast") optional() action { (_, c) =>
      c.copy(failFast = true)
    } textW ("Stop on first error. Otherwise keep adding next files. Default is false.")

    noteW("\nArguments:")
    arg[Seq[File]]("<files or directories>") unbounded() action { (x, c) =>
      c.copy(files = c.files ++ x)
    } validate { fs =>
      fs.find(d => !d.exists) match {
        case Some(f) => failure(s"Path `${f.path}' does not exist!")
        case _ => success
      }
    } text ("One or more existing files or directories.")

  }

  def exec(cfg: Config, opts: Opts): Unit = withLock(cfg) {
    val apiAdd = CheeApi(cfg).addFiles(ResultCount.empty, resultCountProgress)_
    val decrypt = opts.cryptOpts.toSettings(cfg)
    apiAdd(AddParam(opts.fileSettings, decrypt, opts.failFast, opts.files))
  }
}

object Add {

  case class Opts(
    fileSettings: FileSettings = FileSettings(),
    failFast: Boolean = false,
    files: Seq[File] = Seq.empty,
    cryptOpts: CryptOpts = CryptOpts()) {

    def update(f: CryptOpts => CryptOpts): Opts =
      copy(cryptOpts = f(cryptOpts))
    def updateF(f: FileSettings => FileSettings): Opts =
      copy(fileSettings = f(fileSettings))
  }

}
