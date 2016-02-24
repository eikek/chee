package chee.properties

case class Property(ident: Ident, value: String) {
  def map(f: Ident => Ident) = Property(f(ident), value)
}

object Property {
  import scala.language.implicitConversions

  implicit def fromTuple1(t: (Ident, String)): Property =
    Property(t._1, t._2)

  implicit def fromTuple2(t: (Symbol, String)): Property =
    Property(t._1, t._2)
}

sealed trait PropertyMap {
  def apply(name: Ident): Option[Property]
  def asSet: Set[Property]
  def add(p: Property): PropertyMap
  def -(id: Ident): PropertyMap
  def isEmpty: Boolean
  def filterKeys(f: Ident => Boolean): PropertyMap
  def mapIdents(f: Ident => Ident): PropertyMap

  def get(name: Ident): Option[String] = apply(name).map(_.value)
  def +(p: Property): PropertyMap = add(p)
  def +?(op: Option[Property]) = op match {
    case Some(p) => add(p)
    case _ => this
  }
  def ++(map: PropertyMap): PropertyMap =
    map.asSet.foldLeft(this) { (map, p) => map + p }
  def nonEmpty: Boolean = !isEmpty
}

object PropertyMap {

  val empty: PropertyMap = new PropMap(Map.empty)

  final class PropMap(val props: Map[Ident, Property]) extends PropertyMap {
    def apply(ident: Ident): Option[Property] =
      props.get(ident)

    def add(p: Property): PropertyMap =
      new PropMap(props + (p.ident -> p))

    def -(id: Ident): PropertyMap =
      new PropMap(props - id)

    lazy val asSet = props.values.toSet

    def isEmpty = props.isEmpty

    def filterKeys(f: Ident => Boolean): PropertyMap =
      new PropMap(props.filterKeys(f))

    def mapIdents(f: Ident => Ident) =
      new PropMap(PropertyMap.mapIdents(props, f))

    override def equals(o: Any): Boolean = o match {
      case m: PropMap => props == m.props
      case _ => false
    }

    override def hashCode: Int = props.hashCode

    override def toString = props.toString
  }

  def apply(ps: Property*): PropertyMap =
    new PropMap(ps.map(p => p.ident -> p).toMap)

  def fromOptions(ps: Option[Property]*): PropertyMap =
    ps.foldLeft(empty){ (m, p) => m +? p }

  private def mapIdents(m: Map[Ident, Property], f: Ident => Ident) =
    m.map { case (k, v) => (f(k), v.map(f)) }
}
