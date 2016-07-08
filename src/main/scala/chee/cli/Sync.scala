package chee.cli

import better.files.File
import Sync._
import CryptOptions.{Opts => CryptOpts}
import chee.CheeApi._
import chee.FileOps.Result
import chee.Timing
import chee.properties._
import chee.query.Progress
import com.typesafe.config.Config

class Sync extends ScoptCommand with LockSupport with ProgressDef {

  type T = Opts
  val defaults = Opts()

  val name = "sync"

  val parser = new Parser with CryptOptions[Opts] with LsOptions[Opts] {
    addFileSettings(_ updateF _)
    addDecryptOptions(_ update _, enableOpt = true)

    noteW("\nSync options:")
    opt[Unit]("fail-fast") optional() action { (_, c) =>
      c.copy(failFast = true)
    } textW ("Stop on first error. Otherwise keep adding next files. Default is false.")

    opt[Unit]("reindex") optional() action { (_, c) =>
      c.copy(reindex = true)
    } textW ("Update all files even if unchanged. Default is false.")

    opt[Unit]("everything") action { (_, c) =>
      c.copy(everything = true)
    } textW("Sync all indexed directories. The `<files or directories>' argument must not be used then.")

    noteW("\nArguments:")
    arg[Seq[File]]("<files or directories>") unbounded() optional() action { (x, c) =>
      c.copy(files = c.files ++ x)
    } validate { fs =>
      fs.find(d => !d.exists) match {
        case Some(f) => failure(s"Path `${f.path}' does not exist!")
        case _ => success
      }
    } textW ("One or more existing files or directories.")

    checkConfig { cfg =>
      (cfg.everything, cfg.files.isEmpty) match {
        case (true, true) => success
        case (false, false) => success
        case _ => failure("Either specify --everything or some files or directories.")
      }
    }
  }

  def exec(cfg: Config, opts: Opts): Unit = withLock(cfg) {
    outln("Syncronise " + opts.files.mkString(", "))
    val apiSync = CheeApi(cfg).syncFiles(ResultCount.empty, resultCountProgress)_
    val decrypt = opts.cryptOpts.toSettings(cfg)
    apiSync(SyncParam(opts.fileSettings, decrypt,
      reindex = opts.reindex, failFast = opts.failFast, all = opts.everything, files = opts.files))
  }
}

object Sync {

  case class Opts(
    fileSettings: FileSettings = FileSettings(),
    failFast: Boolean = false,
    reindex: Boolean = false,
    everything: Boolean = false,
    files: Seq[File] = Seq.empty,
    cryptOpts: CryptOpts = CryptOpts()) {

    def update(f: CryptOpts => CryptOpts): Opts =
      copy(cryptOpts = f(cryptOpts))

    def updateF(f: FileSettings => FileSettings): Opts =
      copy(fileSettings = f(fileSettings))
  }

}
