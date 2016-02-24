package chee.properties

import scala.util._
import org.scalatest._

class ValueTest extends FlatSpec with Matchers {
  "like" should "work like a glob for strings" in {

    Value.like("Nikon", "Ni*") should be (Right(true))
    Value.like("Nikon D200", "*200") should be (Right(true))
    Value.like("Nikon D200", "*D*") should be (Right(true))
    Value.like("{{abcde]", "{{ab*e]") should be (Right(true))
    Value.like("abcdefg", "AB*FG") should be (Right(true))
    Value.like("abc", "ABC") should be (Right(true))
  }
}
