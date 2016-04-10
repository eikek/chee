package chee.cli

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

object Encrypt extends ScoptCommand with AbstractLs with CryptCommand {

  val name = "encrypt"

  val parser = new Parser with LsOptions[Opts] with CryptOptions[Opts] with ProcessingOptions[Opts] {
    addLsOptions((c, f) => c.copy(lsOpts = f(c.lsOpts)))
    concurrent() action { (_, c) => c.copy(parallel = true) }
    addEncryptOptions((c, f) => c.copy(cryptOpts = f(c.cryptOpts)))

    queryArg((c, f) => c.copy(lsOpts = f(c.lsOpts)))
  }

  def findPublicKey(cfg: Config, opts: CryptOptions.Opts): PGPPublicKey = {
    val keyFile = opts.keyFile.getOrElse(cfg.getFile("chee.crypt.public-key-file"))
    val keyId = opts.keyId.getOrElse(cfg.getString("chee.crypt.key-id"))
    if (!keyFile.exists) userError(s"Key file `${keyFile}' does not exists!")
    if (keyId.isEmpty) userError("No keyId specified!")
    KeyFind.findPublicKey(keyFile, keyId)
  }

  def processingAction(cfg: Config, opts: CryptOptions.Opts): MapGet[Boolean] = {
    import Processing._
    val sqlite = new SqliteBackend(cfg.getIndexDb)
    opts.cryptMethod.getOrElse(cfg.getCryptMethod) match {
      case CryptMethod.Password =>
        outln("Using password based encryption.")
        val pass = findPassphrase(cfg, opts.passPrompt, opts.passphrase)
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
}
