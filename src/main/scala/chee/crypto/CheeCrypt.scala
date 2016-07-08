package chee.crypto

import better.files._
import chee.properties._
import chee.properties.MapGet._
import chee.util.files._

object CheeCrypt {

  val passwordEncryptExtension = "pbc"
  val publicKeyEncryptExtension = "pkc"

  /** The default filename used to indicate a password encrypted
    * file. It is a sibling of the original file. */
  val passwordEncryptedFile: MapGet[File] = {
    path.map(_.mapFileName(_ + "." + passwordEncryptExtension))
  }

  /** The default filename used to indicate a public-key encrypted
    * file. It is a sibling of the original file. */
  val publicKeyEncryptedFile: MapGet[File] = {
    path.map(_.mapFileName(_ + "." + publicKeyEncryptExtension))
  }

  val decryptFile: MapGet[File] =
    path.map(_.mapFileName(n => n.substring(0, n.length -4)))

  val isPasswordEncrypted: MapGet[Boolean] = {
    path.map(_.getExtension).map {
      _ == Some(passwordEncryptExtension)
    }
  }

  val isPublicKeyEncrypted: MapGet[Boolean] = {
    path.map(_.getExtension).map {
      _ == Some(publicKeyEncryptExtension)
    }
  }

  val isEncrypted: MapGet[Boolean] =
    or(List(isPasswordEncrypted, isPublicKeyEncrypted))

  val isNotEncrypted: MapGet[Boolean] =
    not(isEncrypted)

  def encryptedExtension(path: Ident = Ident.path): MapGet[Option[String]] =
    value(path).map {
      case Some(p) =>
        if (p.endsWith("."+ passwordEncryptExtension)) Some(passwordEncryptExtension)
        else if (p.endsWith("."+ publicKeyEncryptExtension)) Some(publicKeyEncryptExtension)
        else None
      case None => None
    }
}
