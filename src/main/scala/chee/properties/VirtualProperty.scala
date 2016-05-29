package chee.properties

import chee.crypto.CheeCrypt
import MapGet._

case class VirtualProperty(ident: Ident, value: VirtualValue) {
  def map(f: Ident => Ident) = VirtualProperty(f(ident), value.map(f))
}

trait VirtualValue {
  def value: MapGet[Option[String]]
  def map(f: Ident => Ident): VirtualValue
}

object VirtualProperty {

  object idents {
    val pixel: Ident = 'pixel
    val encrypted: Ident = 'encrypted
    val all = List(pixel, encrypted)
  }

  val defaults = new Collection()

  final class Collection(mapping: Ident => Ident = identity) {
    private def pixelValue: VirtualValue = new VirtualValue {
      def value = intValue(mapping('width)).combine(intValue(mapping('height))) {
        case (Some(Right(wp)), Some(Right(hp))) =>
          Some((wp * hp).toString)
        case _ => None
      }
      def map(f: Ident => Ident) = mapIdents(f).pixelValue
    }

    private def encryptedValue: VirtualValue = new VirtualValue {
      import CheeCrypt._
      def value = MapGet.value(mapping(Ident.path)).map {
        case Some(path) =>
          if (path.endsWith("."+ passwordEncryptExtension)) Some(passwordEncryptExtension)
          else if (path.endsWith("."+ publicKeyEncryptExtension)) Some(publicKeyEncryptExtension)
          else None
        case None => None
      }
      def map(f: Ident => Ident) = mapIdents(f).encryptedValue
    }

    val pixel = VirtualProperty(idents.pixel, pixelValue)

    val encrypted = VirtualProperty(idents.encrypted, encryptedValue)

    val all = List(pixel, encrypted)

    def aliasValue(id: Ident): VirtualValue = new VirtualValue {
      def value = MapGet.value(mapping(id))
      def map(f: Ident => Ident) = mapIdents(f).aliasValue(id)
    }

    def alias(p: (Ident, Ident)) = VirtualProperty(p._1, aliasValue(p._2))

    private def mapIdents(f: Ident => Ident): Collection =
      new Collection(mapping andThen f)
  }
}
