package chee.properties

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import chee.PropGen._

object ConditionSpec extends Properties("Condition") {

  property("map(id)(cond) == cond") = forAll { (cond: Condition) =>
    Condition.mapAll(identity)(cond) == cond
  }

}
