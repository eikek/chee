package chee.cli

import better.files._
import chee.cli.CryptOptions.{Opts => CryptOpts}
import chee.cli.LsOptions.{Opts => LsOpts}
import chee.conf._
import chee.util.files._
import chee.properties._
import chee.properties.MapGet._
import chee.query.Progress
import chee.CheeApi
import chee.crypto.CryptMethod
import com.typesafe.config.Config

trait CryptCommand { self: ScoptCommand with AbstractLs =>

  case class Opts(
    lsOpts: LsOpts = LsOptions.Opts(),
    cryptOpts: CryptOpts = CryptOptions.Opts(),
    parallel: Boolean = false) {
    def updateCryptOpts(f: CryptOpts => CryptOpts) =
      copy(cryptOpts = f(cryptOpts))
    def updateLsOpts(f: LsOpts =>  LsOpts) =
      copy(lsOpts = f(lsOpts))
  }

  type T = Opts

  val defaults = Opts()

  lazy val parser = new Parser with LsOptions[Opts] with CryptOptions[Opts] with ProcessingOptions[Opts] {

    addLsOptions(_ updateLsOpts _)

    note(s"\n${name.capitalize} options:")
    concurrent() action { (_, c) => c.copy(parallel = true) }
    if (name == Decrypt.name) {
      addDecryptOptions(_ updateCryptOpts _, title = None)
    } else {
      addEncryptOptions(_ updateCryptOpts _, title = None)
    }

    addQuery(_ updateLsOpts _)
  }

  def progress(parallel: Boolean) = Progress.seq[Unit, Int](
    Progress.before(valueForce(Ident.filename).map { f =>
      if (! parallel) out(s"${name.capitalize}ing $f … ")
    }),
    Progress.after { n =>
      if (!parallel) outln("done")
      else out(".")
      n + 1
    },
    Progress.done { (n, dur) =>
      val msg = s"${name.capitalize}ed $n files in ${chee.Timing.format(dur)}"
      logger.trace(msg)
      if (parallel) out("\n")
      outln(msg)
    }
  )

  def runProcess(props: Stream[LazyMap], processor: MapGet[Boolean], parallel: Boolean): Unit = {
    val action = unit(())
    val prog = progress(parallel)
    if (parallel) {
      prog.foreach(0)(MapGet.parfilter(props, processor), action)
    } else {
      prog.foreach(0)(MapGet.filter(props, processor), action)
    }
  }

  def processingAction(cfg: Config, opts: CryptOptions.Opts): MapGet[Boolean] 

  def exec(cfg: Config, opts: Opts): Unit = {
    val proc = processingAction(cfg, opts.cryptOpts)
    val props = find(cfg, opts.lsOpts)
    runProcess(props, proc, opts.parallel)
  }
}

object CryptCommand {

  /** Get the password in order:
    *  - from `passphrase` if non-empty
    *  - from `cmdKey` which points to a system command in the config file
    *  - from `fileKey' which points to a filename in the config file
    */
  def getPassword(cfg: Config, passphrase: Option[File], cmdKey: String, fileKey: String): Option[Array[Char]] =
    passphrase.flatMap(_.readPassword).orElse {
      cfg.readPasswordFromCommand(cmdKey)
    } orElse {
      cfg.readPasswordFromFile(fileKey)
    }

  /** Finds the passphrase for password-based encryption */
  def findPassphrase(cfg: Config, passPrompt: Boolean, passphrase: Option[File], prompt: String = "Passphrase: "): Array[Char] = {
    val p = passPrompt match {
      case true => Some(promptPassphrase(prompt))
      case _ => getPassword(cfg, passphrase,
        "chee.crypt.default-passphrase-command",
        "chee.crypt.default-passphrase-file")
    }
    p.getOrElse(promptPassphrase(prompt))
  }

  def pubKeySecret(cfg: Config, opts: CryptOpts): Option[CheeApi.PubkeySecret] = {
    val method = opts.cryptMethod.getOrElse(cfg.getCryptMethod)
    method match {
      case CryptMethod.Password => None
      case _ =>
        val keyFile = opts.keyFile.getOrElse(cfg.getFile("chee.crypt.secret-key-file"))
        val pass = getPassword(cfg, opts.secretKeyPass,
          "chee.crypt.secret-key-pass-command",
          "chee.crypt.secret-key-pass-file").getOrElse {
          promptPassphrase("Passphrase for private key: ")
        }
        Some(CheeApi.PubkeySecret(keyFile, pass))
    }
  }

  def passphraseSecret(cfg: Config, opts: CryptOpts) = {
    val method = opts.cryptMethod.getOrElse(cfg.getCryptMethod)
    method match {
      case CryptMethod.Pubkey => None
      case _ => Some(findPassphrase(cfg, opts.passPrompt, opts.passphrase, "Password for decryption: "))
    }
  }

}
