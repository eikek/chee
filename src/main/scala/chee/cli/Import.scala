package chee.cli

import CryptOptions.{Opts => CryptOpts}
import Import._
import better.files.File
import chee.CheeApi._
import com.typesafe.config.Config

class Import extends ScoptCommand with ProgressDef {

  type T = Opts
  val defaults = Opts()

  val name = "import"

  val parser = new Parser with CryptOptions[Opts] with LsOptions[Opts] {
    addFileSettings(_ updateF _)
    addDecryptOptions(_ update _, enableOpt = true)

    noteW("\nImport options:")
    opt[Unit]("fail-fast") optional() action { (_, c) =>
      c.copy(failFast = true)
    } textW ("Stop on first error. Otherwise keep adding next files. Default is false.")
    opt[Unit]("duplicates") action { (_, c) =>
      c.copy(duplicates = true)
    } text ("Import files that already exists in some location.")

    noteW("\nArguments")
    arg[File]("<files...>") unbounded() required() action { (x, c) =>
      c.copy(files = c.files :+ x)
    } textW ("The files or directories to import. The last is the target. "+
      "The target must be a directory.")

    checkConfig { cfg =>
      cfg.files.size match {
        case 0 => failure("No files given.")
        case 1 => failure("No target directory given")
        case n =>
          if (cfg.files.last.isDirectory) success
          else failure(s"The target is not a directory: ${cfg.files.last}")
      }
    }
  }

  def exec(cfg: Config, opts: Opts): Unit = {
    val apiImport = CheeApi(cfg).importFiles(ResultCount.empty, resultCountProgress)_
    val decrypt = opts.cryptOpts.toSettings(cfg)
    apiImport(ImportParam(opts.fileSettings, decrypt,
      failFast = opts.failFast,
      duplicates = opts.duplicates,
      targetDir = opts.files.last,
      files = opts.files.init))
  }
}

object Import {
  case class Opts(
    fileSettings: FileSettings = FileSettings(),
    cryptOpts: CryptOpts = CryptOpts(),
    failFast: Boolean = false,
    duplicates: Boolean = false,
    files: Seq[File] = Seq.empty) {

    def update(f: CryptOpts => CryptOpts): Opts =
      copy(cryptOpts = f(cryptOpts))

    def updateF(f: FileSettings => FileSettings): Opts =
      copy(fileSettings = f(fileSettings))
  }
}
