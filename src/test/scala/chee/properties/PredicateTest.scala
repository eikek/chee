package chee.properties

import scala.util._
import org.scalatest._
import chee.properties.Ident._

class PredicateTest extends FlatSpec with Matchers {
  import Predicates._

  val p1 = Property(filename, "test")
  val p2 = Property(filename, "tset")

  val p3 = Property(make, "Nikon")
  val p4 = Property(model, "D200")
  val p5 = Property(Ident.length, "123423")

  "identprop" should "compare like prop" in {
    val pp1 = identprop(IdentProp(Comp.Gt, Ident.length, Ident.lastModified))
    val pp2 = prop(Prop(Comp.Gt, Ident.length -> "00223233"))
    val map = LazyMap(p5, Ident.lastModified -> "00223233")
    pp2.result(map) should be (false)
    pp1.result(map) should be (pp2.result(map))
  }

  "PropPredicate" should "compare strings" in {
    val pp = prop(Prop(Comp.Eq, p1))
    pp.result(LazyMap(p1)) should be (true)
  }

  it should "compare ints" in {
    val pp = prop(Prop(Comp.Gt, width -> "35"))
    pp.result(LazyMap(width -> "100")) should be (true)
    pp.result(LazyMap(width -> "10")) should be (false)
  }

  "JuncPredicate" should "compare based on operator" in {
    junc(Junc.And, Predicates.True, Predicates.True).result(LazyMap()) should be (true)
    junc(Junc.And, Predicates.False, Predicates.True).result(LazyMap()) should be (false)
    junc(Junc.And, Predicates.True, Predicates.False).result(LazyMap()) should be (false)
    junc(Junc.And, Predicates.False, Predicates.False).result(LazyMap()) should be (false)

    junc(Junc.Or, Predicates.True, Predicates.True).result(LazyMap()) should be (true)
    junc(Junc.Or, Predicates.False, Predicates.True).result(LazyMap()) should be (true)
    junc(Junc.Or, Predicates.True, Predicates.False).result(LazyMap()) should be (true)
    junc(Junc.Or, Predicates.False, Predicates.False).result(LazyMap()) should be (false)
  }

  "NotPredicate" should "inverse" in {
    Predicates.not(Predicates.True).result(LazyMap()) should be(false)
    Predicates.not(Predicates.False).result(LazyMap()) should be(true)
  }

  "ExistsPredicate" should "check for existing keys" in {
    Predicates.exists(Ident.length).result(LazyMap(Ident.length -> "10")) should be (true)
    Predicates.exists(Ident.length).result(LazyMap(width -> "10")) should be (false)
  }

  "Compound conditions" should "evaluate correcty" in {
    import Condition._
    val tree1 = and(Exists(created), lt(width -> "3000"))
    val map1 = LazyMap(created -> "2015-01-01:10:00:03", width -> "241")

    Predicates(tree1).result(map1) should be (true)
    Predicates(tree1).result(map1 add (width -> "5000")) should be (false)
    Predicates(tree1).result(LazyMap(width -> "50")) should be (false)

    Predicates(Condition.not(tree1)).result(map1) should be (false)
    Predicates(Condition.not(tree1)).result(map1 add(width -> "5000")) should be (true)
    Predicates(Condition.not(tree1)).result(LazyMap(width -> "50")) should be (true)

    val tree2 = and(Exists('width), Exists('created), Exists('height))
    val map2 = map1 + (Ident.width -> "2000") + (Ident.height -> "1000")
    Predicates(tree2).result(map1) should be (false)
    Predicates(tree2).result(map2) should be (true)

    val tree3 = or(tree2.nodes: _*)
    Predicates(tree3).result(map1) should be (true)
    Predicates(tree3).result(map2) should be (true)
    Predicates(tree3).result(LazyMap(Ident.path -> "/a/b")) should be (false)
  }

  it should "eval with short circuit" in {
    val badPred: MapGet[Boolean] = MapGet(m => sys.error("should not be called"))

    val pa = junc(Junc.And, Predicates.False, badPred)
    val po = junc(Junc.Or, Predicates.True, badPred)

    pa.result(LazyMap()) should be (false)
    po.result(LazyMap()) should be (true)
  }

  it should "eval to True if empty" in {
    Predicates(Condition.and()) should be (True)
    Predicates(Condition.or()) should be (True)
    Predicates(Condition.and(Condition.or())).result(LazyMap.empty) should be (true)
  }

  "invalid trees" should "throw" in {
    val Failure(_) = Try(Predicates(Prop(Comp("abc"), width -> "10")))
  }
}
