package chee.properties

import chee.metadata.MetadataFile
import org.scalatest._
import chee.TestInfo

class FormatPatternTest extends FlatSpec with Matchers {
  import Patterns._

  val emptyMap = LazyMap()

  "Quote" should "quote strings but not numbers" in {
    quote('"', raw("hello")).right.result(emptyMap) should be ("\"hello\"")
    quote('"', raw("3424")).right.result(emptyMap) should be ("3424")
  }

  it should "escape quote chars" in {
    quote('"', raw("\"hello\", they said")).right.result(emptyMap) should be ("\"\\\"hello\\\", they said\"")
    quote(''', raw("'hello', they said")).right.result(emptyMap) should be ("'\\'hello\\', they said'")
  }

  "fixedwidth" should "pad with space" in {
    fixedwidth(10, raw("14"), frontPad = true).right.result(emptyMap) should be ("        14")
    fixedwidth(10, raw("14"), frontPad = false).right.result(emptyMap) should be ("14        ")
    fixedwidth(10, raw(""), frontPad = true).right.result(emptyMap) should be ("          ")
  }

  it should "cut long strings" in {
    fixedwidth(3, raw("hello")).right.result(emptyMap) should be ("he…")
    fixedwidth(20, raw("/home/photos/2015/02/02/20150202-133212_DSC1.jpg")).right.result(emptyMap) should be (
      "/home/ph…12_DSC1.jpg")
  }

  "maxLen" should "keep short lines" in {
    maxlen(10, raw("12345")).right.result(emptyMap) should be ("12345")
    maxlen(5, raw("12345")).right.result(emptyMap) should be ("12345")
  }

  it should "cut off longer strings at end" in {
    maxlen(5, raw("12345abcde")).right.result(emptyMap) should be ("12345")
  }

  it should "cut longer strings at beginning" in {
    maxlen(5, raw("12345abcde"), false).right.result(emptyMap) should be ("abcde")
  }


  "Value" should "lookup values" in {
    Patterns.readable('length).right.result(LazyMap(Ident.length -> "423123122")) should be ("403.5mb")
    lookup('length).right.result(LazyMap(Ident.length -> "121212")) should be ("121212")
  }

  "Loop" should "loop through all idents" in {
    val patf: Ident => Pattern =
      id => seq(raw("test: "), Patterns.readable('ident), raw(" -> "), Patterns.readable(id))

    val image = chee.TestInfo.images.find(_.name == "test1.jpg").get
    val lastmod = DateTime(image.lastModifiedTime).format("yyyy-MM-dd HH:mm:ss")
    val lmap = LazyMap.fromFile(image, MetadataFile.empty)
    val expect = s"test: path -> ${TestInfo.baseDir.path}/src/test/resources/images/test1.jpg, " +
    "test: filename -> test1.jpg, " +
    "test: length -> 303.8kb, " +
    s"test: lastmodified -> ${lastmod}, " +
    "test: mimetype -> image/jpeg, " +
    "test: extension -> jpg, " +
    "test: checksum -> b6bdc5b62c489ebfa55738fb41a88133cdd52ee0785baf4ccdcc26bcd62a736e, " +
    "test: make -> NIKON CORPORATION, " +
    "test: model -> NIKON 1 J3, " +
    "test: width -> 1900, " +
    "test: height -> 1267, " +
    "test: iso -> 160, " +
    "test: orientation -> 1, " +
    "test: created -> 2014-06-12 20:31:23, " +
    "test: pixel -> 2.4mp, " +
    "test: encrypted -> , " +
    "test: tag -> , " +
    "test: comment -> "

    Patterns.loop(patf, id => raw(", "), MapGet.idents(true)).right.result(lmap) should be (expect)
  }

  it should "remove 'ident key after body generation" in {
    val lp = Patterns.loop(id => lookup('ident), id => raw(", "), MapGet.idents(false), includeEmpty = true)
    val (next, _) = lp.right.run(LazyMap.fromFile(TestInfo.images.head))
    next.propertyKeys(Ident("ident")) should be (false)
  }

  "lookup" should "expand idents" in {
    lookup('file).right.result(LazyMap(Ident.filename -> "test.jpg")) should be ("test.jpg")

  }

  it should "return error if ident cannot be expanded" in {
    lookup('blabla).result(emptyMap) should be a ('left)
  }

  "exisitsIdent" should "return false on non-existing keys" in {
    existsIdent(Ident.extension, true).right.result(LazyMap(Ident.filename -> "test.png")) should be (false)
  }
}
