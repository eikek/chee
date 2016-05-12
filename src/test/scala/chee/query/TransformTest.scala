package chee.query

import org.scalatest._
import chee.properties._
import chee.Collection
import chee.metadata._

class TransformTest extends FlatSpec with Matchers {

  val now = LocalDateTime(2016, 4, 21, 12, 0, 0)

  "PrefixTransform" should "replace valid prefixes" in {
    val tree = Condition.and(Prop(Comp.Eq, Ident("len") -> "32"), Exists(Ident("w")))
    val tsm = new PrefixIdentTransform(Set(Ident.length, Ident.width, Ident.filename))
    tsm(tree) should be (Condition.and(Prop(Comp.Eq, Ident.length -> "32"), Exists(Ident.width)))
    Ident.findIdent(Set(Ident.filename, Ident.extension), Ident("ext")) should be (Right(Ident.extension))
  }

  "CollectionMacro" should "replace simple queries" in {
    val tree = Condition.and(Prop(Comp.Like, Ident("collection") -> "coll1"), Prop(Comp.Gt, Ident.width -> "2000"))

    val colls = Seq(Collection("coll1", "model:Nikon*"))
    val trans = Transform.withCollectionMacro(now, colls)
    trans(tree) should be (Condition.and(Prop(Comp.Like, Ident.model -> "Nikon*"), Prop(Comp.Gt, Ident.width -> "2000")))
  }

  it should "expand idents inside collection definition" in {
    val tree = Condition.and(Prop(Comp.Like, Ident("collection") -> "coll1"), Prop(Comp.Gt, Ident.width -> "2000"))
    val colls = Seq(Collection("coll1", "ext:jpg"))
    val trans = Transform.withCollectionMacro(now, colls)
    trans(tree) should be (Condition.and(Prop(Comp.Like, Ident.extension -> "jpg"), Prop(Comp.Gt, Ident.width -> "2000")))
  }

  it should "expand macros inside collection definition" in {
    val tree = Condition.and(Prop(Comp.Like, Ident("collection") -> "coll1"), Prop(Comp.Gt, Ident.width -> "2000"))
    val colls = Seq(Collection("coll1", "id:abcde"))
    val trans = Transform.withCollectionMacro(now, colls)
    trans(tree) should be (Condition.and(
      Prop(Comp.Like, Ident.checksum -> "abcde*"),
      Prop(Comp.Gt, Ident.width -> "2000")))
  }

  it should "expand collections in collections recursively" in {
    val tree = Condition.and(Prop(Comp.Like, Ident("collection") -> "coll2"), Prop(Comp.Gt, Ident.width -> "2000"))
    val colls = Seq(
      Collection("coll1", "id:abc"),
      Collection("coll2", "coll:coll1")
    )
    val trans = Transform.withCollectionMacro(now, colls)
    trans(tree) should be (Condition.and(
      Prop(Comp.Like, Ident.checksum -> "abc*"),
      Prop(Comp.Gt, Ident.width -> "2000")))
  }

  "range macro" should "recoginze simple ranges" in {
    val tree = Prop(Comp("/"), 'len -> "100--500")
    val trans = new RangeMacro(now)
    trans(tree) should be (
      Condition.and(
        Prop(Comp.Gt, 'len -> "100"),
        Prop(Comp.Lt, 'len -> "500"))
    )
  }

  "id macro" should "replace id idents" in {
    IdMacro(Prop(Comp.Like, 'id -> "abe")) should be (
      Prop(Comp.Like, Ident.checksum -> "abe*"))
  }

  "metadata macro" should "replace non existing tags and comment props into false cond" in {
    val q = new MetadataMacro(_ => Nil)
    q(Prop(Comp.Like, 'tag -> "x")) should be (
      Not(TrueCondition))
    q(Prop(Comp.Like, 'comment -> "y")) should be (
      Not(TrueCondition))
  }

  it should "replace existing tags and comment props with in condition" in {
    val q = new MetadataMacro(_ => Seq("abc", "123", "xyz"))

    q(Prop(Comp.Like, 'tag -> "x")) should be (
      In(Ident.checksum, Seq("abc", "123", "xyz")))

    q(Prop(Comp.Like, 'comment -> "x")) should be (q(Prop(Comp.Like, 'tag -> "x")))
  }

  it should "replace existing tags/comments in exists" in {
    val q = new MetadataMacro(_ => Seq("abc"))
    q(Exists('tag)) should be (
      In(Ident.checksum, Seq("abc")))
  }

  it should "replace existing tags/comments in IN cond" in {
    val q = new MetadataMacro(_ => Seq("123", "abc"))
    q(In('tag, Seq("1", "x"))) should be (
      In(Ident.checksum, Seq("123", "abc")))
  }
}
