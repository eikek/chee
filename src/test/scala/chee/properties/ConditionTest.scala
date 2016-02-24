package chee.properties

import org.scalatest._
import chee.properties.Ident._

class ConditionTest extends FlatSpec with Matchers {
  import Condition._

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
}
