package chee.properties

import better.files._

sealed trait LazyMap { self =>
  /** Lookup a property. If virtual, the value is computed into a normal property. */
  final def apply(ident: Ident): (LazyMap, Option[Property]) = getVirtual(ident) match {
    case Some(vp) => vp.value.value.map(v => v.map(Property(ident, _))).run(this)
    case None => get(ident)
  }

  def add(p: Property): LazyMap

  def get(id: Ident): (LazyMap, Option[Property])

  def addVirtual(vp: VirtualProperty): LazyMap

  def getVirtual(id: Ident): Option[VirtualProperty]

  def remove(ident: Ident): LazyMap

  def mapIdents(f: Ident => Ident): LazyMap

  def virtualKeys: Set[Ident]
  def propertyKeys: Set[Ident]

  def append(other: LazyMap): LazyMap = new LazyMap {
    def add(p: Property): LazyMap =
      (self add p) ++ other

    def get(id: Ident): (LazyMap, Option[Property]) =
      self.get(id) match {
        case (m, r@Some(_)) => (m ++ other, r)
        case (m, None) =>
          other.get(id) match {
            case (o, r@Some(_)) => (m ++ o, r)
            case (o, None) => (m ++ o, None)
          }
      }

    def addVirtual(vp: VirtualProperty): LazyMap =
      (self.addVirtual(vp) ++ other)

    def getVirtual(id: Ident): Option[VirtualProperty] =
      self.getVirtual(id) orElse other.getVirtual(id)

    def remove(ident: Ident): LazyMap =
      (self remove ident) ++ (other remove ident)

    def virtualKeys: Set[Ident] =
      self.virtualKeys ++ other.virtualKeys

    def propertyKeys: Set[Ident] =
      self.propertyKeys ++ other.propertyKeys

    def mapIdents(f: Ident => Ident): LazyMap =
      (self mapIdents f) ++ (other mapIdents f)
  }

  def add(p: Option[Property]): LazyMap =
    p.map(add).getOrElse(this)

  def + (p: Property): LazyMap = add(p)
  def ++ (o: LazyMap): LazyMap = append(o)
}

object LazyMap {
  type VirtualMap = Map[Ident, VirtualProperty]

  def fromFile(f: File, extr: Seq[Extraction] = Extraction.all): LazyMap = {
    val map = new FromFile(f, extr, PropertyMap.empty, Set.empty, Map.empty)
    VirtualProperty.defaults.all.foldLeft(map) { (m, vp) =>
      m.addVirtual(vp)
    }
  }

  def apply(m: PropertyMap): LazyMap =
    VirtualProperty.defaults.all.foldLeft(new ConstantMap(m, Map.empty)) { (m, vp) =>
      m.addVirtual(vp)
    }

  def apply(ps: Property*): LazyMap =
    apply(PropertyMap(ps: _*))

  private final class FromFile(val file: File, val extr: Seq[Extraction], val map: PropertyMap, val seen: Set[Ident], val vmap: VirtualMap) extends LazyMap {
    def get(ident: Ident): (LazyMap, Option[Property]) = {
      if (seen.contains(ident)) (this, map(ident))
      else {
        extr.find(_.idents contains ident) match {
          case None => (this, None)
          case Some(e) =>
            val (next0, props) = e.extractM(file).run(this)
            val next = next0 match {
              case m: FromFile =>
                new FromFile(m.file, m.extr, m.map ++ props, m.seen ++ e.idents, m.vmap)
              case _ =>
                sys.error("extractors should not change maps")
            }
            (next, props(ident))
        }
      }
    }
    lazy val virtualKeys: Set[Ident] = vmap.keySet
    lazy val propertyKeys: Set[Ident] = extr.flatMap(_.idents).toSet ++ map.asSet.map(_.ident)
    def add(p: Property) =
      new FromFile(file, extr, map + p, seen + p.ident, vmap)
    def remove(id: Ident) =
      new FromFile(file, extr, map - id, seen - id, vmap - id)
    def addVirtual(vp: VirtualProperty) =
      new FromFile(file, extr, map, seen, vmap + (vp.ident -> vp))
    def getVirtual(id: Ident) = vmap.get(id)
    def mapIdents(f: Ident => Ident) =
      new FromFile(file, extr.map(_.mapIdents(f)), map.mapIdents(f), seen.map(f), vmap.mapIdents(f))
  }

  private final class ConstantMap(map: PropertyMap, vmap: VirtualMap) extends LazyMap {
    def add(p: Property) =
      new ConstantMap(map + p, vmap)
    def get(id: Ident) = (this, map(id))
    lazy val virtualKeys: Set[Ident] = vmap.keySet
    lazy val propertyKeys: Set[Ident] = map.asSet.map(_.ident)
    def addVirtual(vp: VirtualProperty) =
      new ConstantMap(map, vmap + (vp.ident -> vp))
    def getVirtual(id: Ident) = vmap.get(id)
    def remove(id: Ident) =
      new ConstantMap(map - id, vmap - id)
    def mapIdents(f: Ident => Ident) =
      new ConstantMap(map.mapIdents(f), vmap.mapIdents(f))
  }

  implicit class VirtualMapOps(vmap: VirtualMap) {
    def mapIdents(f: Ident => Ident) =
      vmap.map { case (k,v) => (f(k), v.map(f))}
  }
}
