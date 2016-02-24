package chee

import org.scalacheck.{Properties, Gen, Arbitrary}
import org.scalacheck.Prop.forAll
import chee.properties._

object PropGen {

  val genIdent: Gen[Ident] = Gen.oneOf(Ident.defaults.toList)

  val genStringIdent: Gen[Ident] = Gen.oneOf(Ident.path, Ident.filename, Ident.extension, Ident.make, Ident.model)

  val stringProperty = for {
    v <- Gen.alphaStr
    n <- genStringIdent
  } yield Property(n, v)

  val intProperty = for {
    v <- Arbitrary.arbitrary[Int]
    n <- Gen.oneOf(Ident.width, Ident.height, Ident.iso)
  } yield Property(n, v.toString)

  val longProperty = for (v <- Arbitrary.arbitrary[Long]) yield Property(Ident.length, v.toString)

  val genProperty = Gen.oneOf(stringProperty, intProperty, longProperty)

  def genPropertyList(len: Int): Gen[List[Property]] = Gen.listOfN(len, genProperty)

  def genPropertyMapOfN(len: Int): Gen[PropertyMap] =
    genPropertyList(len).map(ps => PropertyMap(ps: _*))

  val genPropertyMap: Gen[PropertyMap] =  Gen.oneOf(
    Gen.const(PropertyMap.empty), Gen.choose(0, 30).flatMap(n => genPropertyList(n).map(ps => PropertyMap(ps: _*)))
  )

  val widthAndHeightGen = for {
    w <- Gen.choose(4, 9000)
    h <- Gen.choose(4, 9000)
  } yield (w, h)


  implicit val arbProperty: Arbitrary[Property] = Arbitrary(genProperty)

  implicit val arbPropertyMap: Arbitrary[PropertyMap] = Arbitrary(genPropertyMap)

  implicit val arbLazyMap: Arbitrary[LazyMap] = Arbitrary(genPropertyMap.map(LazyMap.apply))


  val genExists: Gen[Exists] = genStringIdent.map(n => Exists(n))

  val genProp: Gen[Prop] = for {
    comp <- Gen.oneOf(Comp.Eq, Comp.Like, Comp.Lt, Comp.Gt)
    p <- genProperty
  } yield Prop(comp, p)

  val genLeaf: Gen[Leaf] = Gen.oneOf(genExists, genProp)


  def genNot(sz: Int): Gen[Not] = for (n <- genCond(sz - 1)) yield Not(n)

  def genJunc(sz: Int): Gen[Junc] = for {
    op <- Gen.oneOf(Junc.Or, Junc.And)
    len <- Gen.choose(sz/3, sz/2)
    nodes <- Gen.listOfN(len, genCond(sz/2))
  } yield Junc(op, nodes)

  def genCond(sz: Int): Gen[Condition] = {
    if (sz == 0) Gen.oneOf(genExists, genProp)
    else Gen.frequency((1, genLeaf), (1, genNot(sz)), (2, genJunc(sz)))
  }

  implicit def arbCondition: Arbitrary[Condition] = Arbitrary(Gen.sized(sz => genCond(sz)))

  implicit def arbDateTime: Arbitrary[DateTime] = Arbitrary {
    for (l <- Arbitrary.arbitrary[Long]) yield DateTime(l)
  }

  implicit def arbLocalDateTime = Arbitrary {
    for {
      year <- Gen.choose(1900, 3000)
      month <- Gen.choose(1, 12)
      day <- Gen.choose(1,31)
      hour <- Gen.choose(0,23)
      min <- Gen.choose(0, 59)
      sec <- Gen.choose(0, 59)
    } yield LocalDateTime(year, month, day, hour, min, sec)
  }
}
