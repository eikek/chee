package chee.query

import org.scalatest._
import chee.properties._
import chee.Collection

class TransformTest extends FlatSpec with Matchers {

  "PrefixTransform" should "replace valid prefixes" in {
    val tree = Condition.and(Prop(Comp.Eq, Ident("len") -> "32"), Exists(Ident("w")))
    val tsm = new PrefixIdentTransform(Set(Ident.length, Ident.width, Ident.filename))
    tsm(tree) should be (Condition.and(Prop(Comp.Eq, Ident.length -> "32"), Exists(Ident.width)))
    Ident.findIdent(Set(Ident.filename, Ident.extension), Ident("ext")) should be (Right(Ident.extension))
  }

  "CollectionMacro" should "replace simple queries" in {
    val tree = Condition.and(Prop(Comp.Like, Ident("collection") -> "coll1"), Prop(Comp.Gt, Ident.width -> "2000"))

    val colls = Seq(Collection("coll1", "model:Nikon*"))
    val trans = Transform.withCollectionMacro(colls)
    trans(tree) should be (Condition.and(Prop(Comp.Like, Ident.model -> "Nikon*"), Prop(Comp.Gt, Ident.width -> "2000")))
  }

  it should "expand idents inside collection definition" in {
    val tree = Condition.and(Prop(Comp.Like, Ident("collection") -> "coll1"), Prop(Comp.Gt, Ident.width -> "2000"))
    val colls = Seq(Collection("coll1", "ext:jpg"))
    val trans = Transform.withCollectionMacro(colls)
    trans(tree) should be (Condition.and(Prop(Comp.Like, Ident.extension -> "jpg"), Prop(Comp.Gt, Ident.width -> "2000")))
  }

  it should "expand macros inside collection definition" in {
    val tree = Condition.and(Prop(Comp.Like, Ident("collection") -> "coll1"), Prop(Comp.Gt, Ident.width -> "2000"))
    val colls = Seq(Collection("coll1", "extension~jpg;png;gif"))
    val trans = Transform.withCollectionMacro(colls)
    trans(tree) should be (Condition.and(
      Condition.or(
        Prop(Comp.Like, Ident.extension -> "jpg"),
        Prop(Comp.Like, Ident.extension -> "png"),
        Prop(Comp.Like, Ident.extension -> "gif")),
      Prop(Comp.Gt, Ident.width -> "2000")))
  }

  it should "expand collections in collections recursively" in {
    val tree = Condition.and(Prop(Comp.Like, Ident("collection") -> "coll2"), Prop(Comp.Gt, Ident.width -> "2000"))
    val colls = Seq(
      Collection("coll1", "ext~jpg;png;gif"),
      Collection("coll2", "coll:coll1")
    )
    val trans = Transform.withCollectionMacro(colls)
    trans(tree) should be (Condition.and(
      Condition.or(
        Prop(Comp.Like, Ident.extension -> "jpg"),
        Prop(Comp.Like, Ident.extension -> "png"),
        Prop(Comp.Like, Ident.extension -> "gif")),
      Prop(Comp.Gt, Ident.width -> "2000")))
  }
}
