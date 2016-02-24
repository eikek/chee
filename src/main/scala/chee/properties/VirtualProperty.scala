package chee.properties

case class VirtualProperty(ident: Ident, value: VirtualValue) {
  def map(f: Ident => Ident) = VirtualProperty(f(ident), value.map(f))
}

trait VirtualValue {
  def value: MapGet[Option[String]]
  def map(f: Ident => Ident): VirtualValue
}

object VirtualProperty {
  import MapGet._

  object idents {
    val pixel: Ident = 'pixel
    val all = List(pixel)
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

    val pixel = VirtualProperty('pixel, pixelValue)

    val all = List(pixel)

    def aliasValue(id: Ident): VirtualValue = new VirtualValue {
      def value = MapGet.value(mapping(id))
      def map(f: Ident => Ident) = mapIdents(f).aliasValue(id)
    }

    def alias(p: (Ident, Ident)) = VirtualProperty(p._1, aliasValue(p._2))

    private def mapIdents(f: Ident => Ident): Collection =
      new Collection(mapping andThen f)
  }
}
