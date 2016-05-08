package chee.metadata

import org.scalacheck._
import org.scalacheck.Arbitrary._
import chee.metadata.RecElement._

object Generators {
  implicit val arbComment: Arbitrary[Comment] =
    Arbitrary(Gen.alphaStr.map(s => Comment(s, 0)))

  def genField(labelPrefix: String = ""): Gen[Field] =
    for {
      label <- Gen.alphaStr.suchThat(_.length > 1)
      value <- Gen.alphaStr
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
