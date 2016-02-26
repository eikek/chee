package chee.query

import org.scalatest._
import chee.properties._

class QueryParserTest extends FlatSpec with Matchers {

  import QueryParser._

  "identprop" should "parse successful" in {
    parseAll(idprop(Comp.all), "width>'height").get should be (
      IdentProp(Comp.Gt, Ident.width, Ident.height)
    )
  }

  "simpleValue" should "parse successful" in {
    parseAll(simpleValue, "test").get should be ("test")
    parseAll(simpleValue, "12-test").get should be ("12-test")
  }

  "simpleValue" should "fail properly" in {
    val Failure(_, _) = parseAll(simpleValue, "tes test")
    val Failure(_, _) = parseAll(simpleValue, "te\"st")
    val Failure(_, _) = parseAll(simpleValue, "te()st")
  }

  "quotedValue" should "parse successful" in {
    parseAll(quotedValue, "\"\\a\"").get should be ("a")
    parseAll(quotedValue, "'ab\\\"cde'").get should be ("ab\"cde")
    parseAll(quotedValue, "'ab\"cde'").get should be ("ab\"cde")
    parseAll(quotedValue, "'ab\\'cde)'").get should be ("ab'cde)")
    parseAll(quotedValue, "'some thing'").get should be ("some thing")
    parseAll(quotedValue, "''").get should be ("")
  }

  "quotedValue" should "fail properly" in {
    val Failure(_, _) = parseAll(quotedValue, "'ab ")
    val Failure(_, _) = parseAll(quotedValue, "eb'")
    val Failure(_, _) = parseAll(quotedValue, "\"eb'")
    val Failure(_, _) = parseAll(quotedValue, "")
  }

  "propValue" should "parse successful" in {
    parseAll(propValue, "jpg").get should be ("jpg")
    parseAll(propValue, "'jpg test'").get should be ("jpg test")
  }

  "prop" should "parse successful" in {
    parseAll(prop(Comp.all.toSet), "ext:jpg").get should be (
      Prop(Comp.Like, Ident("ext") -> "jpg"))
  }

  "exists" should "parse successful" in {
    parseAll(exists, "ext?").get should be (Exists(Ident("ext")))
  }

  "not" should "parse successful" in {
    val tries = Map(
      "!ext:jpg" -> Not(Prop(Comp.Like, Ident("ext") -> "jpg")),
      "!ext?" -> Not(Exists(Ident("ext"))),
      "!!!!ext:jpg" -> Not(Not(Not(Not(Prop(Comp.Like, Ident("ext") -> "jpg"))))))

    for ((str, tree) <- tries) {
      parseAll(QueryParser.not(Comp.all.toSet), str).get should be (tree)
      parseAll(QueryParser.condition(Comp.all.toSet), str).get should be (tree)
    }
  }

  "juncOp" should "parse successful" in {
    parseAll(juncOp, "&").get should be (Junc.And)
    parseAll(juncOp, "|").get should be (Junc.Or)
  }

  "junc" should "parse successful" in {
    val parsed = parseAll(junc(Comp.all.toSet), "(& ext:jpg file:bla*)")
    parsed.get should be (
      Condition.and(
        Prop(Comp.Like, Ident("ext") -> "jpg"),
        Prop(Comp.Like, Ident("file") -> "bla*")))
  }

  "conditions" should "parse successfully" in {
    val tries = Map(
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
      "(& !ext:rest !ext:test)" -> Condition.and(
        Not(Prop(Comp.Like, Ident("ext") -> "rest")),
        Not(Prop(Comp.Like, Ident("ext") -> "test"))))

    for ((str, tree) <- tries) {
      QueryParser(str) should be (Right(tree))
    }
  }

  "query" should "do postprocessing" in {
    val tree = Query.create(QuerySettings(LocalDateTime.now))("!!(& ext:jpg file:bla*)")
    tree should be (Right(Condition.and(
      Prop(Comp.Like, Ident.extension -> "jpg"),
      Prop(Comp.Like, Ident.filename -> "bla*"))))
  }
}
