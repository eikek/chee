package chee.cli

import chee.Processing
import chee.crypto.{CheeCrypt, CryptMethod, KeyFind}
import chee.conf._
import chee.properties._
import chee.properties.MapGet._
import chee.query.SqliteBackend
import com.typesafe.config.Config
import org.bouncycastle.openpgp.PGPPublicKey
import CryptCommand._

class Encrypt extends ScoptCommand with AbstractLs with CryptCommand {

  val name = "encrypt"

  def findPublicKey(cfg: Config, opts: CryptOptions.Opts): PGPPublicKey = {
    val keyFile = opts.keyFile.getOrElse(cfg.getFile("chee.crypt.public-key-file"))
    val keyId = opts.keyId.getOrElse(cfg.getString("chee.crypt.key-id"))
    if (!keyFile.exists) userError(s"Key file `${keyFile}' does not exists!")
    if (keyId.isEmpty) userError("No keyId specified!")
    KeyFind.findPublicKey(keyFile, keyId)
  }

  def processingAction(cfg: Config, opts: CryptOptions.Opts): MapGet[Boolean] = {
    import Processing._
    val sqlite = new SqliteBackend(cfg)
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
