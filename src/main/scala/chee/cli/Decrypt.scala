package chee.cli

import better.files._
import chee.Processing
import chee.crypto.CryptMethod
import chee.conf._
import chee.properties._
import chee.properties.MapGet._
import chee.query.SqliteBackend
import com.typesafe.config.Config

object Decrypt extends ScoptCommand with AbstractLs with CryptCommand {

  val name = "decrypt"

  def decryptFile(cfg: Config, opts: CryptOptions.Opts, out: MapGet[File]): MapGet[Boolean] = {
    val method = opts.cryptMethod.getOrElse(cfg.getCryptMethod)
    val decryptSecret = method match {
      case CryptMethod.Password => None
      case _ =>
        val keyFile = opts.keyFile.getOrElse(cfg.getFile("chee.crypt.secret-key-file"))
        val pass = getPassword(cfg, opts.secretKeyPass,
          "chee.crypt.secret-key-pass-command",
          "chee.crypt.secret-key'pass-file").getOrElse {
          promptPassphrase("Passphrase for private key: ")
        }
        Some(Processing.DecryptSecret(keyFile, pass))
    }
    val passphrase = method match {
      case CryptMethod.Pubkey => None
      case _ => Some(findPassphrase(cfg, opts.passPrompt, opts.passphrase, "Password for decryption: "))
    }
    Processing.decryptFile(decryptSecret, passphrase, out)
  }

  def processingAction(cfg: Config, opts: CryptOptions.Opts): MapGet[Boolean] = {
    import Processing._
    val sqlite = new SqliteBackend(cfg.getIndexDb)
    val out = path.map(_.mapFileName(n => n.substring(0, n.length -4)))
    this.decryptFile(cfg, opts, out).flatMap {
      case true => cryptInplacePostProcess(sqlite)
      case false => unit(false)
    }
  }
}
