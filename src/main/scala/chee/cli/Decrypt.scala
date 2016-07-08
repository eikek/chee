package chee.cli

import better.files._
import chee.crypto.CheeCrypt
import chee.properties.MapGet
import chee.query.SqliteBackend
import CryptOptions.{Opts => CryptOpts}
import CryptCommand._
import chee.Processing
import chee.Processing._
import com.typesafe.config.Config

class Decrypt extends ScoptCommand with AbstractLs with CryptCommand {

  val name = Decrypt.name

  def processingAction(cfg: Config, opts: CryptOptions.Opts): MapGet[Boolean] = {
    val sqlite = new SqliteBackend(cfg)
    val out = CheeCrypt.decryptFile
    Decrypt.decryptFile(cfg, opts, out).flatMap {
      case true => cryptInplacePostProcess(sqlite)
      case false => MapGet.unit(false)
    }
  }
}

object Decrypt {
  val name = "decrypt"

  def decryptFile(cfg: Config, opts: CryptOpts, out: MapGet[File]): MapGet[Boolean] = {
    val decryptSecret = pubKeySecret(cfg, opts)
    val passphrase = passphraseSecret(cfg, opts)
    Processing.decryptFile(decryptSecret, passphrase, out)
  }
}
