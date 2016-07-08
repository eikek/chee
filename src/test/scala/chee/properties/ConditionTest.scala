package chee.properties

import org.scalatest._
import chee.properties.Ident._
import ConditionFormat._
import Condition._

class ConditionTest extends FlatSpec with Matchers {

  def Not(c: Condition) = Condition.not(c)

  "normalize" should "remove empty juncs 1" in {
    val tree1 = and(or(and(), and()))
    normalize(tree1) should be (and())
    normalize(and(or(and(and(),and(and(or(and(), and()))))))) should be (and())
  }

  it should "remove empty juncs 2" in {
    val tree2 = and(or(Exists(Ident.width), Exists(Ident.height)), and())
    normalize(tree2) should be (or(Exists(Ident.width), Exists(Ident.height)))
    normalize(or(tree2, and(or(), and()))) should be (or(Exists(Ident.width), Exists(Ident.height)))
  }

  it should "remove double negation" in {
    val t1 = and(Not(Not(Exists(width))))
    normalize(t1) should be (Exists(Ident.width))
    normalize(Not(Not(Not(Exists(width))))) should be (Not(Exists(width)))
    normalize(Not(Not(Not(Not(Exists(width)))))) should be (Exists(width))
  }

  "prop" should "render to id:value" in {
    render(Prop(Comp.Like, Ident.path -> "x")) should be ("path:'x'")
    render(Prop(Comp.Eq, Ident.path -> "x")) should be ("path='x'")
  }

  "exists" should "render to x?" in {
    render(Exists(Ident.path)) should be ("path?")
  }

  "identprop" should "render to x>'y" in {
    render(IdentProp(Comp.Gt, Ident.width, Ident.height)) should be ("width>'height")
  }

  "in" should "render to x~a;b;c" in {
    render(In(Ident.extension, List("a", "b", "c"))) should be ("extension~'a;b;c'")
  }

  it should "escape ' and ;" in {
    render(In(Ident.extension, List("a;z", "b;z", "c"))) should be ("extension~'a\\;z;b\\;z;c'")
    render(In(Ident.extension, List("a'z", "b'z", "c"))) should be ("extension~'a\\'z;b\\'z;c'")
  }

  "lookup" should "create props correctly" in {
    val p1 = Condition.lookup(Comp.Like, Ident.make, Ident.model)
    val map = LazyMap(
      Ident.make -> "nikon",
      Ident.model -> "nikon")
    Predicates(p1.result(map)).result(map) should be (true)
    Predicates(p1.result(map + (Ident.model -> "nik"))).result(map) should be (false)

    val p2 = Condition.lookup(Ident.length)
    val map2 = LazyMap(Ident.length -> "10")
    Predicates(p2.result(map2)).result(map2) should be (true)

    val p3 = Condition.lookup(Comp.Gt, Ident.length, Ident.iso)
    val map3 = LazyMap(Ident.length -> "5", Ident.iso -> "2")
    Predicates(p3.result(map3)).result(map3) should be (true)
    val map4 = LazyMap(Ident.length -> "5", Ident.iso -> "20")
    Predicates(p3.result(map4)).result(map4) should be (false)
  }

  it should "check for non-existing values" in {
    val p1 = Condition.lookup(Ident.length)
    val map1 = LazyMap()
    Predicates(p1.result(map1)).result(map1) should be (true)

    val p2 = Condition.lookup(Comp.Gt, Ident.length)
    Predicates(p2.result(map1)).result(map1) should be (false)
  }
}
