package chee.cli

import better.files._
import chee.crypto.{ Algorithm, CheeCrypt, KeyFind }
import chee.query.{ Progress, SqliteBackend }
import com.typesafe.config.Config
import chee.CheeConf.Implicits._
import chee.CheeConf.CryptMethod
import chee.Size
import chee.properties._
import chee.properties.MapGet._
import chee.Processing
import org.bouncycastle.openpgp.PGPPublicKey

object Decrypt extends AbstractLs with CryptProgress {

  val name = "decrypt"

  case class Opts(
    lsOpts: LsOpts = LsOpts(),
    parallel: Boolean = false,
    cryptMethod: Option[CryptMethod] = None,
    passphrase: Option[Array[Char]] = None,
    passPrompt: Boolean = false,
    keyFile: Option[File] = None,
    secretKeyPass: Option[Array[Char]] = None
  ) extends CommandOpts

  type T = Opts

  val defaults = Opts()

  val parser = new LsOptionParser {
    def copyLsOpts(o: Opts, lso: LsOpts) = o.copy(lsOpts = lso)

    def moreOptions(): Unit = {
      opt[Unit]('c', "concurrent") action { (_, c) =>
        c.copy(parallel = true)
      } text("Process files concurrently.")

      opt[CryptMethod]("method") valueName("password|pubkey") action { (m, c) =>
        c.copy(cryptMethod = Some(m))
      } text ("The encryption method: either pubkey or password. Using public\n"+
        "        key encryption requires a public key that must be specified in\n"+
        "        the config file or via options. For password-based encryption\n"+
        "        a passphrase must be specified (via config file or options).\n"+
        "        If one method is specified, files encrypted with the other \n"+
        "        method are not touched. If this option is not specified, both\n"+
        "        methods are used.")

      opt[Unit]('W', "passprompt") action { (_, c) =>
        c.copy(passPrompt = true)
      } text ("Always prompt for a passphrase. Do not use the default-\n"+
        "        passphrase from in the config file. Only applicable when\n"+
        "        password-based encryption is used.")

      opt[File]("key-file") valueName("<file>") action { (f, c) =>
        c.copy(keyFile = Some(f))
      } text ("The file containing the secret key. A key-id must also be\n"+
        "        specified. The openpgp formats (ascii and binary) can be used.")

      opt[String]("secret-key-pass") action { (p, c) =>
        c.copy(secretKeyPass = Some(p.toCharArray()))
      } text ("The passphrase to access the private key. If not specified, it\n"+
        "        is prompted for.")

      opt[String]("passphrase") action { (p, c) =>
        c.copy(passphrase = Some(p.toCharArray()))
      } text ("Specify a passphrase to use for password-based encryption. The\n"+
        "        `-W' option overrides this.")
    }
  }

  def processingAction(cfg: Config, opts: Opts): MapGet[Boolean] = {
    import Processing._
    val sqlite = new SqliteBackend(cfg.getIndexDb)
    val out = path.map(_.mapFileName(n => n.substring(0, n.length -4)))
    val decryptSecret = opts.cryptMethod match {
      case Some(CryptMethod.Password) => None
      case _ =>
        val keyFile = opts.keyFile.getOrElse(cfg.getFile("chee.crypt.secret-key-file"))
        val pass = opts.secretKeyPass.getOrElse(promptPassphrase("Passphrase for private key: "))
        Some(DecryptSecret(keyFile, pass))
    }
    val passphrase = opts.cryptMethod match {
      case Some(CryptMethod.Pubkey) => None
      case _ => Some(findPassphrase(cfg, opts.passPrompt, opts.passphrase, "Password for decryption: "))
    }
    decryptFile(decryptSecret, passphrase, out).flatMap {
      case true => cryptInplacePostProcess(sqlite)
      case false => unit(false)
    }
  }

  def exec(cfg: Config, opts: Opts, props: Stream[LazyMap]): Unit = {
    val proc = processingAction(cfg, opts)
    val action = unit(())
    val prog = progress("Decrypt", opts.parallel)
    if (opts.parallel) {
      prog.foreach(0)(MapGet.parfilter(props, proc), action)
    } else {
      prog.foreach(0)(MapGet.filter(props, proc), action)
    }
  }
}
