package chee.cli

import better.files.File
import chee.crypto.CheeCrypt
import chee.properties._
import chee.conf._
import chee.util.files._
import com.typesafe.config.Config

/** Enable transparent decryption.
  *
  * It is an addon for the {{AbstractLs}} mixin. If a command mixes
  * this trait in, it can handle encrypted as follows:
  *
  * - If the user requests decryption, encrypted files are decrypted
  *   to a temporary location and the properties will reflect the
  *   decrypted files. The result of `find` does not contain any
  *   encrypted files. This is useful when applying action to images
  *   (i.e. view) that are encrypted.
  * - If the user does not requests decryption, all encrypted files
  *   are removed from the result.
  *
  * Either way the result of `find` never contains encrypted files.
  * The command must provide options to the user to set the
  * appropriate information (key-file, passphrase) for decrypting.
  */
trait TransparentDecrypt { self: AbstractLs =>

  def findDecrypt(cfg: Config, lsOpts: LsOptions.Opts, cryptOpts: CryptOptions.Opts): Stream[LazyMap] = {
    if (cryptOpts.enable) {
      val decryptAction = Decrypt.decryptFile(cfg, cryptOpts, TransparentDecrypt.tempDecrypt(cfg))
      val filter = CheeCrypt.isEncrypted.flatMap {
        case true => decryptAction
        case false => MapGet.unit(true)
      }
      MapGet.filter(find(cfg, lsOpts), filter)
    } else {
      // filter encrypted files by applying a query condition
      val opts = lsOpts.appendQuery(s"!${VirtualProperty.idents.encrypted.name}?")
      find(cfg, opts)
    }
  }

}

object TransparentDecrypt {

  def tempDecrypt(cfg: Config): MapGet[File] = {
    val tempdir = cfg.getFile("chee.crypt.decrypt-temp")
    if (!tempdir.exists) tempdir.createDirectories()
    MapGet.pair(MapGet.valueForce(Ident.checksum), MapGet.path).map {
      case (hash, path) =>
        val ext = path.stripExtension.getExtension.map("." + _).getOrElse("")
        tempdir / s"$hash$ext"
    }
  }
}
