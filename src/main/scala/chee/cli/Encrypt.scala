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

object Encrypt extends AbstractLs {

  val name = "encrypt"

  case class Opts(
    lsOpts: LsOpts = LsOpts(),
    parallel: Boolean = false,
    cryptMethod: Option[CryptMethod] = None,
    passphrase: Option[Array[Char]] = None,
    passPrompt: Boolean = false,
    keyFile: Option[File] = None,
    keyId: Option[String] = None,
    algorithm: Option[Algorithm] = None
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
        "        a passphrase must be specified (via config file or options).")

      opt[Unit]('W', "passprompt") action { (_, c) =>
        c.copy(passPrompt = true)
      } text ("Always prompt for a passphrase. Do not use the default-\n"+
        "        passphrase from in the config file. Only applicable when\n"+
        "        password-based encryption is used.")

      opt[File]("key-file") valueName("<file>") action { (f, c) =>
        c.copy(keyFile = Some(f))
      } text ("The file containing the public key. A key-id must also be\n"+
        "        specified. The openpgp formats (ascii and binary) can be used.")

      opt[String]("key-id") action { (k, c) =>
        c.copy(keyId = Some(k))
      } text ("A key id matching a public key in the `key-file'. Can be part\n"+
        "        of the user-id or key-id and must uniquely identify a key.")

      opt[String]("passphrase") action { (p, c) =>
        c.copy(passphrase = Some(p.toCharArray()))
      } text ("Specify a passphrase to use for password-based encryption. The\n"+
        "        `-W' option overrides this.")
    }
  }

  val progress = Progress.seq[Unit, Int](
    Progress.after { n => n + 1},
    Progress.done { (n, dur) =>
      val msg = s"Encrypted $n files in ${chee.Timing.format(dur)}"
      logger.trace(msg)
      outln(msg)
    }
  )

  def findPublicKey(cfg: Config, opts: Opts): PGPPublicKey = {
    val keyFile = opts.keyFile.getOrElse(cfg.getFile("chee.crypt.public-key-file"))
    val keyId = opts.keyId.getOrElse(cfg.getString("chee.crypt.key-id"))
    if (!keyFile.exists) userError(s"Key file `${keyFile}' does not exists!")
    if (keyId.isEmpty) userError("No keyId specified!")
    KeyFind.findPublicKey(keyFile, keyId)
  }

  def findPassphrase(cfg: Config, opts: Opts): Array[Char] = {
    val p = if (opts.passPrompt) {
      promptPassphrase()
    } else opts.passphrase.getOrElse {
      cfg.getString("chee.crypt.default-passphrase").toCharArray
    }
    if (p.isEmpty) promptPassphrase()
    else p
  }

  def processingAction(cfg: Config, opts: Opts): MapGet[Boolean] = {
    import Processing._
    val sqlite = new SqliteBackend(cfg.getIndexDb)
    opts.cryptMethod.getOrElse(cfg.getCryptMethod) match {
      case CryptMethod.Password =>
        outln("Using password based encryption.")
        val pass = findPassphrase(cfg, opts)
        encryptPassword(pass, cfg.getAlgorithm, CheeCrypt.passwordEncryptedFile).flatMap {
          case true => cryptInplacePostProcess(sqlite)
          case false => unit(false)
        }
      case CryptMethod.Pubkey =>
        outln("Using public key encryption.")
        val key = findPublicKey(cfg, opts)
        encryptPubkey(key, CheeCrypt.publicKeyEncryptedFile).flatMap {
          case true => cryptInplacePostProcess(sqlite)
          case false => unit(false)
        }
    }
  }

  def exec(cfg: Config, opts: Opts, props: Stream[LazyMap]): Unit = {
    val proc = processingAction(cfg, opts)
    if (opts.parallel) {
      val action = MapGet.get.map(m => out("."))
      progress.foreach(0)(MapGet.parfilter(props, proc), action)
    } else {
      val action = MapGet.valueForce(Ident.filename).map(f => outln(s"Encrypted $f"))
      progress.foreach(0)(MapGet.filter(props, proc), action)
    }
  }
}
