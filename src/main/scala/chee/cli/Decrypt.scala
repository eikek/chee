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

object Decrypt extends ScoptCommand with AbstractLs with CryptCommand {

  val name = "decrypt"

  val parser = new Parser with LsOptions[Opts] with CryptOptions[Opts] with ProcessingOptions[Opts] {
    addLsOptions((c, f) => c.copy(lsOpts = f(c.lsOpts)))
    concurrent() action { (_, c) => c.copy(parallel = true) }
    addDecryptOptions((c, f) => c.copy(cryptOpts = f(c.cryptOpts)))

    queryArg((c, f) => c.copy(lsOpts = f(c.lsOpts)))
  }

  def decryptFile(cfg: Config, opts: CryptOptions.Opts, out: MapGet[File]): MapGet[Boolean] = {
    val decryptSecret = opts.cryptMethod match {
      case Some(CryptMethod.Password) => None
      case _ =>
        val keyFile = opts.keyFile.getOrElse(cfg.getFile("chee.crypt.secret-key-file"))
        val pass = opts.secretKeyPass.getOrElse(promptPassphrase("Passphrase for private key: "))
        Some(Processing.DecryptSecret(keyFile, pass))
    }
    val passphrase = opts.cryptMethod match {
      case Some(CryptMethod.Pubkey) => None
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
