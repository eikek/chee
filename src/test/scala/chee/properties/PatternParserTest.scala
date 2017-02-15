package chee.properties

import org.scalatest._
import fastparse.all._
import chee.util.parsing._
import chee.TestInfo

class PatternParserTest extends FlatSpec with Matchers {
  import Patterns._

  val parser = new PatternParser(Ident.defaults)
  val emptyMap = LazyMap()

  import parser._

  def parseVal(p: P[Ident => Pattern], in: String): MapGet[String] = {
    p.parseAll(in) match {
      case Right(dir) => dir('value).right
      case f => MapGet.unit(f.toString)
    }
  }

  "simpleDirective" should "parse raw values" in {
    parseVal(simpleDirective, "abcde").result(emptyMap) should be ("abcde")
    parseVal(simpleDirective, "ab~~cde").result(emptyMap) should be ("ab~cde")
    parseVal(simpleDirective, "ab~~cde :this to").result(emptyMap) should be ("ab~cde :this to")
    parseVal(simpleDirective, "ab~~cde ~~ :this ~~to").result(emptyMap) should be ("ab~cde ~ :this ~to")
    parseVal(simpleDirective, "~~ab~~cde :").result(emptyMap) should be ("~ab~cde :")
    parseVal(simpleDirective, "~~").result(emptyMap) should be ("~")
  }

  "simpleDirective" should "parse identifier lookups" in {
    val map = emptyMap.add(Ident.filename -> "test.jpg")
    parseVal(simpleDirective, "~#filename").result(map) should be ("test.jpg")
    parseVal(simpleDirective, "~:filename").result(map) should be ("test.jpg")
  }

  "ValueDirective" should "accept format strings" in {
    val now = DateTime.now
    val map = emptyMap +
    (Ident.created -> "2015-12-22 22:14:04") +
    (Ident.added -> now.instant.toString) +
    (Ident.lastModified -> now.instant.toString) +
    (Ident.iso -> "200") +
    (Ident.width -> "1000") +
    (Ident.height -> "400") +
    (Ident.length -> "21") +
    (Ident("origin-length") -> "21")

    def parse(s: String) = parseVal(lookupValue, s).result(map)

    parse("~#iso~f%05d") should be ("00200")
    parse("~#height~f%05d") should be ("00400")
    parse("~#width~f%05d") should be ("01000")
    parse("~#length~f%05d") should be ("00021")
    parse("~#origin-length") should be ("21")
    parse("~#created~fyyyy") should be ("2015")
    parse("~#added~fyyyy") should be (now.format("yyyy"))
    parse("~#lastmodified~fyyyy") should be (now.format("yyyy"))
  }

  "quoted" should "parse successful" in {
    val map = emptyMap + (Ident.filename -> "test.jpg") + (Ident.iso -> "200")
    parseVal(quotedValue, "~'~#filename").result(map) should be ("'test.jpg'")
    parseVal(quotedValue, "~\"~#filename").result(map) should be ("\"test.jpg\"")
    parseVal(quotedValue, "~'~#iso").result(map) should be ("200")
    parseVal(quotedValue, "~\"~#iso").result(map) should be ("200")
    //does not really make much sense, but …
    parseVal(quotedValue, "~'ab").result(map) should be ("'ab'")
    parseVal(quotedValue, "~\"ab").result(map) should be ("\"ab\"")
  }

  "sequence" should "parse successful" in {
    val map = emptyMap + (Ident.filename -> "test.jpg") + (Ident.iso -> "200")
    parseVal(sequence, "~{~:filename: ~#iso~}").result(map) should be ("test.jpg: 200")
    parseVal(quotedValue, "~'~{~:filename:~}").result(map) should be ("'test.jpg:'")
  }

  "conditional" should "parse successful" in {
    val map = LazyMap.fromFile(TestInfo.images.find(_.name.endsWith("png")).get)
    parseVal(conditional, "~[model~;~:model~;nil~]").result(map) should be ("nil")
    parseVal(conditional, "~[filename~;~:filename~;nil~]").result(map) should be ("nixos_logo.png")
  }

  "loopBody" should "combine different patterns" in {
    val Right((mf, sf)) = loopBody.parseAll("size: ~:value")
    mf('length).result(LazyMap(Ident.length -> "12311222")) should be (Right("size: 11.7mb"))
  }

  "loopDirective" should "parse successful" in {
    val map = emptyMap + (Ident.filename -> "test.jpg") + (Ident.iso -> "200")
    parseVal(loopDirective, "~@!~{~:value ~}").result(map) should (
      be ("test.jpg 200 ") or be ("200 test.jpg ")
    )
    parseVal(loopDirective, "~@!~{~:ident -> ~:value~^, ~}").result(map) should (
      be ("filename -> test.jpg, iso -> 200") or be ("iso -> 200, filename -> test.jpg"))

    val str = parseVal(loopDirective, "~@*~{~:ident -> ~:value~^, ~}").result(map)
    for (id <- (map.propertyKeys ++ map.virtualKeys)) {
      str should include (s"${id.name} ->")
    }
    str should include ("filename -> test.jpg")
    str should include ("iso -> 200")
  }

  it should "work with condition directive" in {
    val map = emptyMap + (Ident.filename -> "test.jpg") + (Ident.iso -> "200")
    parseVal(loopDirective, "~@!~{~[value~;~:value~;nil~]~}").result(map) should (
      be ("200test.jpg") or be ("test.jpg200")
    )
  }

  "lengthDirective" should "parse successful" in {
    val map = emptyMap + (Ident.filename -> "test.jpg") + (Ident.iso -> "200") + (Ident.checksum -> "d5a017bbd8603632d9630ec5a405ab23d86860deaaffa90545b8")
    parseVal(parser.length, "~20l~{test: ~:filename~}").result(map) should be (
      "      test: test.jpg")
    parseVal(parser.length, "~20r~{test: ~:filename~}").result(map) should be (
     "test: test.jpg      ")
    parseVal(parser.length, "~20l~{test: ~:filename test2: ~:iso~}").result(map) should be (
      "test: te… test2: 200")

    parseVal(parser.length, "~8mr~:checksum").result(map) should be ("d5a017bb")
    parseVal(parser.length, "~8ml~:checksum").result(map) should be ("a90545b8")
  }

  "controlString" should "parse successful" in {
    val map = LazyMap(
      Ident.filename -> "test.jpg",
      Ident.width -> "1920",
      Ident.height -> "1200",
      Ident.length -> "111000",
      Ident.path -> "/home/pictures/2014/winter/test.jpg",
      Ident.created -> "2012-10-14 14:22:11"
    )
    val format = "~19r~:created ~9r~{~#width~.x~.~#height~} ~[length~;~:length~;no-size~] ~:filename"
    val expected = "2012-10-14 14:22:11 1920x1200 108.4kb test.jpg"
    parseVal(parser.lift(controlString), format).result(map) should be (expected)
    parseVal(parser.lift(controlString), "~#created~fyyyy MM~. test").result(map) should be ("2012 10 test")
    parseVal(parser.lift(controlString), ">~20l~#path<").result(map) should be (">/home/pi…er/test.jpg<")
  }

  "parse" should "expand idents" in {
    val map = emptyMap + (Ident.filename -> "test.jpg") + (Ident.iso -> "200")
    val tests = Map(
      "~:file" -> "test.jpg",
      "~[file~;~:file~;~:is~]" -> "test.jpg",
      "~'~:file" -> "'test.jpg'",
      "~{~:file ~:iso~}" -> "test.jpg 200")

    for ((pattern, str) <- tests) {
      parsePattern(pattern).right.get.right.result(map) should be (str)
    }

    parsePattern("~@!~{~:ident ~:value~^, ~}").right.get.right.result(map) should (
      be ("filename test.jpg, iso 200") or be ("iso 200, filename test.jpg")
    )
  }
}
