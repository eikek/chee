package chee.metadata

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalatest._
import fastparse.all.P
import chee.properties.{ FormatPatterns, MapGet }
import chee.util.parsing._
import chee.util.Render.Ops
import RecElement._
import RecFormat._
import Generators._

class RecParserTest extends FlatSpec with Matchers {

  val dp = new DatabaseParser()
  val mp = new MapParser()

  def checkMetadataFile(db: Database): Unit = {
    val Some(r1) = db.records.find(_.get("Checksum").exists(_.value == "f3f"))
    r1.valueOf("Checksum") should be (Vector("f3f"))
    r1.valueOf("Tag") should be (Vector("summer", "holidays"))
    r1.valueOf("Comment") should be (Vector("A comment to the image."))

    val Some(r2) = db.records.find(_.get("Checksum").exists(_.value == "e53"))
    r2.valueOf("Checksum") should be (Vector("e53"))
    r2.valueOf("Tag") should be (Vector("spring", "car", "swimming"))
    r2.valueOf("Comment") should have size (1)
    r2.valueOf("Comment").headOption match {
      case Some(str) =>
        str should include ("quam, a auctor enim")
        str should include ("eget, sodales eget")
        str should include ("sem. Aenean est diam")
      case _ => sys.error("")
    }
  }

  def metadataFile(num: Int): String =
    io.Source.fromURL(getClass.getResource(s"/metadata${num}.rec")).mkString

  "lineWrap" should "map \\\\n into \"\"" in {
    RecElParser.lineWrap.parseAll("\\\n") should be (Right(""))
  }

  "multiline" should "do" in {
    RecElParser.newLine.parseAll("\n+ ") should be (Right("\n"))
    RecElParser.newLine.parseAll("\n+") should be (Right("\n"))
  }

  "value" should "parse multiline and linewraps" in {
    RecElParser.value.parseAll("abcd") should be (Right("abcd"))
    RecElParser.value.parseAll("\n+ \n+ ") should be (Right("\n\n"))
    RecElParser.value.parseAll("abcd\\\ndef") should be (Right("abcddef"))
    RecElParser.value.parseAll("abcd\\\ndef\n+ 123") should be (Right("abcddef\n123"))
  }

  "field" should "parse multi lines" in {
    RecElParser.field.parseAll("Address: Rimourstreet 15\n+ 7444 Signapur") should be (
      Right(Field("Address", "Rimourstreet 15\n7444 Signapur", 0)))
  }

  it should "parse linewraps" in {
    RecElParser.field.parseAll("Address: Rimourstreet 15\\\n 7444 Signapur") should be (
      Right(Field("Address", "Rimourstreet 15 7444 Signapur", 0)))
  }

  "record" should "parse records" in {
    RecElParser.recordEl.parseAll("Tag: bike") should be (Right(
      Field("Tag", "bike", 0)))
    RecElParser.recordEl.parseAll("Checksum: u\\\nx") should be (Right(
      Field("Checksum", "ux", 0)))
    RecElParser.recordEl.parseAll("Checksum: u\n+x") should be (Right(
      Field("Checksum", "u\nx", 0)))
  }

  "descriptor" should "parse records consisting of %-fields" in {
    RecElParser.descrEl.parseAll("%rec: bike") should be (Right(
      Field("%rec", "bike", 0)))
    RecElParser.descrEl.parseAll("%rec: u\\\nx") should be (Right(
      Field("%rec", "ux", 0)))
    RecElParser.descrEl.parseAll("%rec: u\n+x") should be (Right(
      Field("%rec", "u\nx", 0)))
  }

  "recordComment" should "parse" in {
    RecElParser.recordComment.parseAll("# hahaha") should be (Right(
      Comment(" hahaha", 0)))
  }

  "db record" should "parse Records" in {
    dp.record.parseAll("# hahaha") should be ('left)
    dp.record.parseAll("Tag: bike") should be (Right(
      Some(Record(Field("Tag", "bike", 0)))))

    dp.record.parseAll("Tag: bike\n# haha") should be (Right(
      Some(Record(Field("Tag", "bike", 0), Comment(" haha", 10)))))

    dp.record.parseAll("Checksum: ab99\nTag: bike") should be (Right(
      Some(Record(Field("Checksum", "ab99", 0), Field("Tag", "bike", 15)))))

    dp.record.parseAll("Checksum: ab99\n# huhu\nTag: bike") should be (Right(
      Some(Record(Field("Checksum", "ab99", 0), Comment(" huhu", 15), Field("Tag", "bike", 22)))))
  }

  "db descriptor" should "parse Descriptors" in {
    dp.descript.parseAll("# hahaha") should be ('left)
    dp.descript.parseAll("%cfg: bike") should be (Right(
      Descriptor(Field("%cfg", "bike", 0))))

    dp.descript.parseAll("%cfg: bike\n# haha") should be (Right(
      Descriptor(Field("%cfg", "bike", 0), Comment(" haha", 11))))

    dp.descript.parseAll("%rec: cfg\n%type: bike") should be (Right(
      Descriptor(Field("%rec", "cfg", 0), Field("%type", "bike", 10))))

    dp.descript.parseAll("%rec: ab99\n# huhu\n%type: bike") should be (Right(
      Descriptor(Field("%rec", "ab99", 0), Comment(" huhu", 11), Field("%type", "bike", 18))))
  }

  "db parser" should "ignore multiple new lines between records" in {
    val Right(db) = dp.parse(metadataFile(1))
    db.records should have size (2)
    db.els should have size (3)

    checkMetadataFile(db)
  }

  it should "parse comments" in {
    val Right(db) = dp.parse(metadataFile(2))
    db.records should have size (2)
    db.els should have size (7)

    checkMetadataFile(db)
  }

  it should "succeed with comments at bottom" in {
    val db0 = Database.Empty + Record(Field("Name", "Silly", 0)) + Comment("hello", 13)
    dp.parse(db0.render) should be (Right(db0))
  }

  it should "succeed for comments only" in {
    val db0 = Database(Vector(
      Comment("aaa", 0),
      Comment("bbb", 6),
      Comment("ccc", 12)))
    dp.parse(db0.render) should be (Right(db0))
  }

  it should "parse multiline comments" in {
    val db0 = Database(Vector(
      Comment("aaa\nbbb", 0),
      Comment("ccc", 11)))
    dp.parse(db0.render) should be (Right(db0))
  }

  "map parser" should "create correct maps" in {
    val Right(lm1) = mp.parse(metadataFile(1))
    val Right(lm2) = mp.parse(metadataFile(2)) // files have same contents besides comments

    val lisp1 = lm1.map(m => FormatPatterns.lisp.result(m)).toList
    val lisp2 = lm2.map(m => FormatPatterns.lisp.result(m)).toList

    lisp1 should be (lisp2)
    lisp1 should have size (2)

    val r1 = lm1.toList.apply(0)
    MapGet.valueForce('checksum).result(r1) should be ("f3f")
    MapGet.valueForce('tag).result(r1) should be ("|holidays|summer|")
    MapGet.valueForce('comment).result(r1) should be ("A comment to the image.")

    val r2 = lm1.toList.apply(1)
    MapGet.valueForce('checksum).result(r2) should be ("e53")
    MapGet.valueForce('tag).result(r2) should be ("|car|spring|swimming|")
    val str = MapGet.valueForce('comment).result(r2)
    str should include ("quam, a auctor enim")
    str should include ("eget, sodales eget")
    str should include ("sem. Aenean est diam")
  }
}


object RecParserSpec extends Properties("RecParser") {

  // set positions to 0
  val zeroPos: RecElement.Entry => Option[RecElement.Entry] = {
    case Record(fs, d, n) => Some(Record(fs.map({
      case Field(l, v, n) => Field(l, v, 0)
      case Comment(t, n) => Comment(t, 0)
    }), Descriptor.Empty ,0))
    case Comment(t, n) => Some(Comment(t, 0))
    case Descriptor.NonEmpty(els, n) => Some(Descriptor.NonEmpty(els.map({
      case Field(l, v, n) => Field(l, v, 0)
      case Comment(t, n) => Comment(t, 0)
    }), 0))
    case n => Some(n)
  }
  val parser = new DatabaseParser(Descriptor.Empty, zeroPos)

  def parse[T](p: P[T], in: String): T = p.parsePrefix(in) match {
    case Right(x) => x
    case Left(m) => sys.error(m)
  }

  property("parse(field.render) == field") = forAll { (f: Field) =>
    parse(RecElParser.field, f.render) == f
  }

  property("parse(comment.render) == comment") = forAll { (c: Comment) =>
    parse(RecElParser.comment, c.render) == c.text
  }

  property("parse(record.render) == record") = forAll { (r: Record) =>
    parse(parser.record, r.render) == Some(r)
  }

  // generator generates only non-empty descriptors
  property("parse(descriptor.render) == descriptor") = forAll { (d: Descriptor) =>
    zeroPos(parse(parser.descript, d.render)) == Some(d)
  }

  property("parse(database.render) == database") = forAll { (db: Database) =>
    parser.parse(db.render) == Right(db)
  }
}
