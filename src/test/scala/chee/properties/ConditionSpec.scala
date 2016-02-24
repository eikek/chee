package chee.properties

import org.scalacheck.{Properties, Gen, Arbitrary}
import org.scalacheck.Prop.forAll

object ConditionSpec extends Properties("Condition") {
  import chee.PropGen._

  property("map(id)(cond) == cond") = forAll { (cond: Condition) =>
    Condition.mapAll(identity)(cond) == cond
  }

  property("reduce terminates") = forAll { (cond: Condition) =>
    countNodes(cond) >= 1
  }

  property("mapLeafs") = forAll { (cond: Condition) =>
    val and = Junc(Junc.And, List(Exists(Ident.filename), Exists(Ident.length)))
    val more = Condition.mapAll({
      case _: Leaf => and
      case c => c
    })(cond)
    val numLeafs = countLeafs(cond)
    countNodes(more) == countNodes(cond) + (2 * numLeafs)
  }

  val countNodes: Condition => Int = Condition.reduce[Int](
    leaf => 1,
    op => (a, b) => a + b,
    op => ob => 1 + ob.getOrElse(0),
    neg => neg + 1
  )

  val countLeafs: Condition => Int = Condition.reduce[Int](
    leaf => 1,
    op => (a, b) => a + b,
    op => ob => ob.getOrElse(0),
    neg => neg
  )
}
