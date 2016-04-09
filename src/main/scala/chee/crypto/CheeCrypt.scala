package chee.crypto

import better.files._
import chee.properties._
import chee.properties.MapGet._

object CheeCrypt {

  val passwortEncryptExtension = "pbc"

  val publicKeyEncryptExtension = "pkc"

  /** The default filename used to indicate a password encrypted
    * file. It is a sibling of the original file. */
  val passwordEncryptedFile: MapGet[File] = {
    import chee.cli.FileExt
    path.map(_.mapFileName(_ + "." + passwortEncryptExtension))
  }

  /** The default filename used to indicate a public-key encrypted
    * file. It is a sibling of the original file. */
  val publicKeyEncryptedFile: MapGet[File] = {
    import chee.cli.FileExt
    path.map(_.mapFileName(_ + "." + publicKeyEncryptExtension))
  }

  val isPasswordEncrypted: MapGet[Boolean] = {
    path.map(_.extension).map {
      _ == Some(passwortEncryptExtension)
    }
  }

  val isPublicKeyEncrypted: MapGet[Boolean] = {
    path.map(_.extension).map {
      _ == Some(publicKeyEncryptExtension)
    }
  }

  val isEncrypted: MapGet[Boolean] =
    Predicates.or(List(isPasswordEncrypted, isPublicKeyEncrypted))

}
