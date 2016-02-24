package chee.properties

import org.scalatest._

class PackageTest extends FlatSpec with Matchers {

  type R = Either[String, String]

  "flatten2" should "combine in correct order" in {
    val els: Seq[(R, R)] = Seq(
      (Right("1"), Right("a")),
      (Right("2"), Right("b")),
      (Right("3"), Right("c"))
    )

    flatten2(els) should be (Right(List(("1", "a"), ("2", "b"), ("3", "c"))))
  }

  it should "return last left" in {
    val els: Seq[(R, R)] = Seq(
      (Right("1"), Right("a")),
      (Right("2"), Left("b")),
      (Left("3"), Right("c"))
    )

    flatten2(els) should be (Left("3"))
  }
}
