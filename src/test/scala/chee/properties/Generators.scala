package chee.properties

import org.scalacheck.{Gen, Arbitrary}
import chee.properties._

object Generators {

  val genComp: Gen[Comp] = Gen.oneOf(Comp.all.toSeq)

  val genIdent: Gen[Ident] = Gen.oneOf(Ident.defaults.toList)

  val genStringIdent: Gen[Ident] = Gen.oneOf(Ident.path, Ident.filename, Ident.extension, Ident.make, Ident.model)

  val stringProperty = for {
    v <- Gen.alphaStr
    n <- genStringIdent
  } yield Property(n, v)

  val intProperty = for {
    v <- Gen.choose(0, 10000)
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
    comp <- genComp
    p <- genProperty
  } yield Prop(comp, p)

  implicit val arbProp: Arbitrary[Prop] = Arbitrary(genProp)


  val genIn: Gen[In] = for {
    id <- genIdent
    values <- Gen.listOf(Gen.listOf(Gen.frequency(
      (9, Gen.alphaNumChar),
      (1, Gen.oneOf(" ;'\"")))).map(_.mkString)).suchThat(_.size > 1)
  } yield In(id, values)

  val genLeaf: Gen[Leaf] = Gen.oneOf(genExists, genProp, genIn)

  def genNot(sz: Int, nonEmpty: Boolean): Gen[Not] = for (n <- genCond(sz - 1, nonEmpty)) yield Not(n)

  def genJunc(sz: Int, nonEmpty: Boolean): Gen[Junc] = for {
    op <- Gen.oneOf(Junc.Or, Junc.And)
    len <- Gen.choose(sz/3, sz/2)
    nodes <- Gen.listOfN(if (nonEmpty && len == 0) 1 else len, genCond(sz/2, nonEmpty))
  } yield Junc(op, nodes)

  def genCond(sz: Int, nonEmpty: Boolean): Gen[Condition] = {
    if (sz < 2) Gen.oneOf(genExists, genProp)
    else Gen.frequency((1, genLeaf), (1, genNot(sz, nonEmpty)), (2, genJunc(sz, nonEmpty)))
  }

  def genNonEmptyCond: Gen[Condition] = {
    for {
      n <- Gen.choose(1, 30)
      cond <- genCond(n, true)
    } yield cond
  }

  implicit def arbCondition: Arbitrary[Condition] = Arbitrary {
    for {
      n <- Gen.choose(0, 30)
      cond <- genCond(n, false)
    } yield cond
  }

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
