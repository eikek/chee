package chee.metadata

import org.scalacheck._
import org.scalacheck.Arbitrary._
import chee.metadata.RecElement._

object Generators {
  val genLabel: Gen[String] = (for {
    alpha <- Gen.alphaChar
    rest <- Gen.listOf(Gen.frequency(
      (9, Gen.alphaNumChar),
      (1, Gen.const('_'))))
    } yield (alpha :: rest)).map(_.mkString)

  def genValue(more: String): Gen[String] =
    Gen.listOf(Gen.frequency(
      (9, Gen.alphaNumChar),
      (2, Gen.const(' ')),
      (1, Gen.oneOf("„“–•@(){}<>[]…_^!&=/*?-:#$|~`+%\"" + more)))).map(_.mkString)

  implicit val arbComment: Arbitrary[Comment] =
    Arbitrary(genValue("\\").map(s => Comment(s, 0)))

  def genField(labelPrefix: String = ""): Gen[Field] =
    for {
      label <- genLabel.suchThat(_.length > 1)
      value <- genValue("\n")
    } yield Field(labelPrefix + label, value, 0)


  implicit val arbField: Arbitrary[Field] = Arbitrary {
    genField()
  }

  def genRecordEl(labelPrefix: String = ""): Gen[RecordEl] =
    Gen.oneOf(arbitrary[Comment], genField(labelPrefix))

  implicit val arbRecord: Arbitrary[Record] = Arbitrary {
    for {
      n <- Gen.choose(1, 20)
      f <- arbitrary[Field]
      els <- Gen.listOfN(n, genRecordEl())
    } yield Record(f +: els.toVector, Descriptor.Empty, 0)
  }

  implicit val arbDescriptor: Arbitrary[Descriptor] = Arbitrary {
    for {
      n <- Gen.choose(1, 20)
      f <- genField("%")
      els <- Gen.listOfN(n, genRecordEl("%"))
    } yield Descriptor.NonEmpty(f +: els.toVector, 0)
  }

  val genEntry: Gen[Entry] = Gen.oneOf(
    arbitrary[Comment],
    arbitrary[Record],
    arbitrary[Descriptor])

  implicit val arbDatabase: Arbitrary[Database] = Arbitrary {
    for {
      n <- Gen.choose(1, 20)
      els <- Gen.listOfN(n, genEntry)
    } yield Database(els.toVector)
  }
}
