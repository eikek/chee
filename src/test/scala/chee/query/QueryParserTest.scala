package chee.query

import org.scalatest._
import chee.properties._
import chee.util.parsing._

class QueryParserTest extends FlatSpec with Matchers {

  val parser = new QueryParser(Comp.all)

  "identprop" should "parse successful" in {
    parser.idprop.parseAll("width>'height") should be (Right(
      IdentProp(Comp.Gt, Ident.width, Ident.height)
    ))
  }

  "simpleValue" should "parse successful" in {
    QueryParser.simpleValue.parseAll("test") should be (Right("test"))
    QueryParser.simpleValue.parseAll("12-test") should be (Right("12-test"))
  }

  "simpleValue" should "fail properly" in {
    val simple = QueryParser.simpleValue
    simple.parseAll("tes test") should be ('left)
    simple.parseAll("te\"st") should be ('left)
    simple.parseAll("te()st") should be ('left)
  }

  "quotedValue" should "parse successful" in {
    val quotedValue = QueryParser.quotedValue
    quotedValue.parseAll("'ab\\\"cde'") should be (Right("ab\"cde"))
    quotedValue.parseAll("'ab\"cde'") should be (Right("ab\"cde"))
    quotedValue.parseAll("'ab\\'cde)'") should be (Right("ab'cde)"))
    quotedValue.parseAll("'some thing'") should be (Right("some thing"))
    quotedValue.parseAll("''") should be (Right(""))
    quotedValue.parseAll("'\\a'") should be (Right("a"))
    quotedValue.parseAll("\"\\a\"") should be (Right("a"))
  }

  "quotedValue" should "fail properly" in {
    val quotedValue = QueryParser.quotedValue
    quotedValue.parseAll("'ab ") should be ('left)
    quotedValue.parseAll("eb'") should be ('left)
    quotedValue.parseAll("\"eb'") should be ('left)
    quotedValue.parseAll("") should be ('left)
  }

  "propValue" should "parse successful" in {
    parser.propValue.parseAll("jpg") should be (Right("jpg"))
    parser.propValue.parseAll("'jpg test'") should be (Right("jpg test"))
  }

  "prop" should "parse successful" in {
    parser.prop.parseAll("ext:jpg") should be (Right(
      Prop(Comp.Like, Ident("ext") -> "jpg")))
  }

  "exists" should "parse successful" in {
    parser.exists.parseAll("ext?") should be (Right(Exists(Ident("ext"))))
  }

  "in" should "parse successful" in {
    parser.in.parseAll("ext~jpg;png;gif") should be (Right(In('ext, Seq("jpg", "png", "gif"))))
    parser.in.parseAll("ext~j\\;pg;png;gi\\;f") should be (Right(In('ext, Seq("j;pg", "png", "gi;f"))))
    parser.in.parseAll("ext~'j p g;pn g;g if'") should be (Right(In('ext, Seq("j p g", "pn g", "g if"))))
  }

  "not" should "parse successful" in {
    val tries = Map(
      "!ext:jpg" -> Not(Prop(Comp.Like, Ident("ext") -> "jpg")),
      "!ext?" -> Not(Exists(Ident("ext"))),
      "!!!!ext:jpg" -> Not(Not(Not(Not(Prop(Comp.Like, Ident("ext") -> "jpg"))))))

    for ((str, tree) <- tries) {
      parser.not.parseAll(str) should be (Right(tree))
      parser.condition.parseAll(str) should be (Right(tree))
    }
  }

  "juncOp" should "parse successful" in {
    parser.juncOp.parseAll("&") should be (Right(Junc.And))
    parser.juncOp.parseAll("|") should be (Right(Junc.Or))
  }

  "junc" should "parse successful" in {
    val parsed = parser.junc.parseAll("(& ext:jpg file:bla*)")
    parsed should be (Right(
      Condition.and(
        Prop(Comp.Like, Ident("ext") -> "jpg"),
        Prop(Comp.Like, Ident("file") -> "bla*"))))
  }

  "conditions" should "parse successfully" in {
    val tries = Map(
      "ext~jpg;png;gif" -> In('ext, Seq("jpg", "png", "gif")),
      "ext:test" -> Prop(Comp.Like, Ident("ext") -> "test"),
      "!ext:test" -> Not(Prop(Comp.Like, Ident("ext") -> "test")),
      "ext?" -> Exists(Ident("ext")),
      "!ext?" -> Not(Exists(Ident("ext"))),
      "!(& ext:jpg file:bla*)" -> Not(Condition.and(
        Prop(Comp.Like, Ident("ext") -> "jpg"),
        Prop(Comp.Like, Ident("file") -> "bla*"))),
      "(& ext? file?)" -> Condition.and(
        Exists(Ident("ext")), Exists(Ident("file"))),
      "(&(|(& ext:jpg)))" -> Condition.and(Condition.or(Condition.and(
        Prop(Comp.Like, Ident("ext") -> "jpg")))),
      "(& !ext:test)" -> Condition.and(
        Not(Prop(Comp.Like, Ident("ext") -> "test"))),
      "width>'height" -> IdentProp(Comp.Gt, Ident.width, Ident.height),
      "(& !ext:rest !ext:test)" -> Condition.and(
        Not(Prop(Comp.Like, Ident("ext") -> "rest")),
        Not(Prop(Comp.Like, Ident("ext") -> "test"))),
      "(& ext~jpg;png)" -> Condition.and(
        In('ext, Seq("jpg", "png"))))

    for ((str, tree) <- tries) {
      parser.parse(str) should be (Right(tree))
    }
  }

  "query" should "do postprocessing" in {
    val tree = Query.create(QuerySettings(LocalDateTime.now))("!!(& ext:jpg file:bla*)")
    tree should be (Right(Condition.and(
      Prop(Comp.Like, Ident.extension -> "jpg"),
      Prop(Comp.Like, Ident.filename -> "bla*"))))
  }
}
