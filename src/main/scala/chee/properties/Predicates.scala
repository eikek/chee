package chee.properties

import better.files._

object Predicates {
  import MapGet._

  type Predicate = MapGet[Boolean]

  val True = unit(true)
  val False = unit(false)

  def exists(id: Ident): MapGet[Boolean] =
    find(id).map(_.isDefined)

  def fileExists: MapGet[Boolean] =
    existingPath.map(_.isDefined)

  def checksumMatch(file: File): Predicate =
    value(Ident.checksum).map { s =>
      s == ChecksumExtract.checksum(file)
    }

  def checksumMatch(m1: LazyMap, m2: LazyMap): (LazyMap, LazyMap, Boolean) = {
    val (nextM1, cs1) = value(Ident.checksum).run(m1)
    val (nextM2, cs2) = value(Ident.checksum).run(m2)
    (nextM1, nextM2, cs1 == cs2)
  }

  def not(p: Predicate): Predicate =
    p.map(b => !b)

  def prop(p: Prop): Predicate = {
    require(Ident.isDefault(p.ident), s"Identifier '${p.ident}' cannot be compared.")
    require(Comp.isDefault(p.comp), s"Comparator '${p.comp} not supported.")
    val conv = Value.forIdent(p.ident)
    value(p.ident).map {
      case None => false
      case Some(v) =>
        conv.evalString(v, p.comp, p.value) match {
          case Right(b) => b
          case Left(err) => sys.error(err)
        }
    }
  }

  def and(ps: List[Predicate]): Predicate =
    ps.toStream.foldRight(unit(true))(combineAnd)

  def or(ps: List[Predicate]): Predicate =
    ps.toStream.foldRight(unit(false))(combineOr)

  def boolCombine(stop: Boolean, f: (Boolean, Boolean) => Boolean)
    (p1: Predicate, p2: Predicate): Predicate = MapGet { m =>
    val (next1, b1) = p1.run(m)
    if (b1 == stop) (next1, b1)
    else {
      val (next2, b2) = p2.run(next1)
      (next2, f(b1, b2))
    }
  }

  val combineOr: (Predicate, Predicate) => Predicate =
    boolCombine(true, _ || _)

  val combineAnd: (Predicate, Predicate) => Predicate =
    boolCombine(false, _ && _)

  def junc(op: Junc.Op, ps: Predicate*): Predicate =
    op match {
      case Junc.And => and(ps.toList)
      case Junc.Or => or(ps.toList)
    }

  def apply(c: Condition): Predicate =
    Condition.reduce[List[Predicate]](
      leaf => leaf match {
        case p: Prop => List(prop(p))
        case e: Exists => List(exists(e.ident))
        case TrueCondition => List(True)
      },
      _ => (l1, l2) => l1 ::: l2,
      op => ol => ol.map(x => List(junc(op, x: _*)))
        .getOrElse(sys.error("Invalid tree: Cannot map an empty junction to a predicate.")),
      l => l.map(not)
    )(c)(0)
}
