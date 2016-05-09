package chee.properties

import org.scalacheck.{Properties, Gen, Arbitrary}
import org.scalacheck.Prop.{forAll, BooleanOperators}
import Generators._
import scala.language.implicitConversions

object PropPredicateSpec extends Properties("PropPredicate") {

  property("string compare <") = forAll { (s1: String, s2: String) =>
    (s1 < s2) ==> (Ident.filename ~ '< ~ s2)(LazyMap(Ident.filename -> s1))
  }

  property("string compare >") = forAll { (s1: String, s2: String) =>
    (s1 > s2) ==> (Ident.filename ~ '> ~ s2)(LazyMap(Ident.filename -> s1))
  }

  property("string compare =") = forAll { (s1: String, s2: String) =>
    (s1 != s2) ==> !((Ident.filename ~ '= ~ s2)(LazyMap(Ident.filename -> s1)))
  }

  property("string compare =") = forAll { (s1: String, s2: String) =>
    (s1 != s2) ==> !((Ident.filename ~ '= ~ s1)(LazyMap(Ident.filename -> s2)))
  }

  property("string compare =") = forAll { (s: String) =>
    (Ident.filename ~ '= ~ s)(LazyMap(Ident.filename -> s))
  }

  property("int compare <") = forAll { (n1: Int, n2: Int) =>
    (n1 < n2) ==> (Ident.width ~ '< ~ n2.toString)(LazyMap(Ident.width -> n1.toString))
  }

  property("int compare >") = forAll { (n1: Int, n2: Int) =>
    (n1 > n2) ==> (Ident.width ~ '> ~ n2.toString)(LazyMap(Ident.width -> n1.toString))
  }

  property("int compare <") = forAll(widthAndHeightGen) { case ((n1, n2)) =>
    (n1 > 9) ==> (Ident.width ~ '< ~ n1.toString)(LazyMap(Ident.width -> "9"))
  }

  property("int compare =") = forAll { (n: Int) =>
    (Ident.width ~ '= ~ n.toString)(LazyMap(Ident.width -> n.toString))
  }

  property("size compare <") = forAll(Gen.posNum[Long], Gen.posNum[Long]) { (n1: Long, n2: Long) =>
    (n1 < n2) ==> (Ident.length ~ '< ~ n2.toString)(LazyMap(Ident.length -> n1.toString))
  }

  property("size compare >") = forAll(Gen.posNum[Long], Gen.posNum[Long]) { (n1: Long, n2: Long) =>
    (n1 > n2) ==> (Ident.length ~ '> ~ n2.toString)(LazyMap(Ident.length -> n1.toString))
  }

  property("size compare =") = forAll(Gen.posNum[Long]) { (n: Long) =>
    (Ident.length ~ '= ~ n.toString)(LazyMap(Ident.length -> n.toString))
  }

  property("DateTime compare <") = forAll { (n1: DateTime, n2: DateTime) =>
    (n1 < n2) ==> (Ident.added ~ '< ~ n2.instant.toString)(LazyMap(Ident.added -> n1.instant.toString))
  }

  property("DateTime compare >") = forAll { (n1: DateTime, n2: DateTime) =>
    (n1 > n2) ==> (Ident.added ~ '> ~ n2.instant.toString)(LazyMap(Ident.added -> n1.instant.toString))
  }

  property("DateTime compare =") = forAll { (n: DateTime) =>
    (Ident.added ~ '= ~ n.instant.toString)(LazyMap(Ident.added -> n.instant.toString))
  }

  property("LocalDateTime compare <") = forAll { (n1: LocalDateTime, n2: LocalDateTime) =>
    (n1 < n2) ==> (Ident.created ~ '< ~ n2.asString)(LazyMap(Ident.created -> n1.asString))
  }

  property("LocalDateTime compare >") = forAll { (n1: LocalDateTime, n2: LocalDateTime) =>
    val nx = n1.copy(year = n1.year + n2.year)
    (Ident.created ~ '> ~ n2.asString)(LazyMap(Ident.created -> nx.asString))
  }

  property("LocalDateTime compare =") = forAll { (n: LocalDateTime) =>
    (Ident.created ~ '= ~ n.asString)(LazyMap(Ident.created -> n.asString))
  }

  property("pixel compare") = forAll(widthAndHeightGen) { case ((n1, n2)) =>
    val map = LazyMap(Ident.width -> n1.toString, Ident.height -> n2.toString).addVirtual(VirtualProperty.defaults.pixel)
    val px = Ident("pixel")
    (px ~ '= ~ (n2 * n1).toString)(map) &&
    (px ~ '< ~ (n1 * n2 + 1).toString)(map) &&
    (px ~ '> ~ "9")(map)
  }

  implicit class PredicateCtor1(n: Ident) {
    def ~ (c: Comp) = new PredicateCtor2(n, c)
  }
  class PredicateCtor2(n: Ident, c: Comp) {
    def ~ (v: String) = Predicates.prop(Prop(c, Property(n, v))).result _
  }
  implicit def symbol2Comp(s: Symbol): Comp = s match {
    case '< => Comp.Lt
    case '> => Comp.Gt
    case '= => Comp.Eq
    case ': => Comp.Like
    case _ => sys.error("wrong comparator")
  }
}
