package chee.metadata

import chee.FileLoan
import chee.properties.{ FormatPatterns, LazyMap, MapGet, TrueCondition }
import org.scalatest.{ FlatSpec, Matchers }
import scala.util.{ Failure, Success, Try }

class MetadataFileTest extends FlatSpec with Matchers with FileLoan {

  "write" should "create a new file" in withNewFile { f =>
    val map = MapGet.seq(
      mapget.setTags(Tag("car"), Tag("bus")),
      mapget.setComment("a comment so fine"),
      mapget.setId("e53")).toMap.result(LazyMap())

    val mf = MetadataFile(f).write(Seq(map))
    val res = mf.find(TrueCondition)
    res should have size (1)
    FormatPatterns.lisp.result(res.head) should be (FormatPatterns.lisp.result(map))
  }

  it should "change tags" in withTags(Tag("car"), Tag("bus")) { mf =>
    val map = mf.find(TrueCondition).toList(0)
    val next = mapget.setTags(Tag("bike")).toMap.result(map)
    val nmf = mf.write(Seq(next))
    nmf.querySize("tag:bus") should be (0)
    nmf.querySize("tag:bike") should be (1)
  }

  it should "change comment" in withComment("brown fox") { mf =>
    val map = mf.find(TrueCondition).toList(0)
    val next = mapget.setComment("blue fox").toMap.result(map)
    val nmf = mf.write(Seq(next))
    nmf.querySize("comment:*brown*") should be (0)
    nmf.querySize("comment:*blue*") should be (1)
  }

  it should "remove tags" in withMetadata("blue fox", Tag("car"), Tag("bus")) { mf =>
    val map = mf.find(TrueCondition).toList(0)
    val next = mapget.removeAllTags.toMap.result(map)
    val nmf = mf.write(Seq(next))
    nmf.querySize("comment:*blue*") should be (1)
    nmf.querySize("tag?") should be (0)
  }

  it should "add new tags" in withMetadata("blue fox", Tag("car"), Tag("bus")) { mf =>
    val map = mf.find(TrueCondition).toList(0)
    val next = mapget.addTags(Tag("moto")).toMap.result(map)
    val nmf = mf.write(Seq(next))
    nmf.querySize("tag:moto") should be (1)
    nmf.querySize("tag:bus") should be (1)
    nmf.querySize("tag:car") should be (1)
  }

  it should "add remove tags" in withMetadata("blue fox", Tag("car"), Tag("bus")) { mf =>
    val map = mf.find(TrueCondition).toList(0)
    val next = mapget.removeTags(Tag("car")).toMap.result(map)
    val nmf = mf.write(Seq(next))
    nmf.querySize("tag:bus") should be (1)
    nmf.querySize("tag:car") should be (0)
  }

  it should "remove comment" in withMetadata("blue fox", Tag("car"), Tag("bus")) { mf =>
    val map = mf.find(TrueCondition).toList(0)
    val next = mapget.setComment("").toMap.result(map)
    val nmf = mf.write(Seq(next))
    nmf.querySize("comment?") should be (0)
    nmf.querySize("tag:car") should be (1)
  }

  it should "not add duplicates" in withNewFile { f =>
    val map0 = MapGet.seq(
      mapget.setTags(Tag("car"), Tag("bus")),
      mapget.setComment("a comment so fine"),
      mapget.setId("e53")).toMap.result(LazyMap())

    val map1 = MapGet.seq(
      mapget.setTags(Tag("cat"), Tag("eagle")),
      mapget.setComment("a movement so great"),
      mapget.setId("e53")).toMap.result(LazyMap())

    val maps = Seq(map0, map1)
    val mf = MetadataFile(f).write(maps)
    val res = mf.find(TrueCondition)
    res should have size (1)
    //first wins, not a hard requirement, but interesting if it would change
    FormatPatterns.lisp.result(res.head) should be (FormatPatterns.lisp.result(map0))
  }

  it should "change multiple records" in withNewFile { f =>
    val map0 = MapGet.seq(
      mapget.setTags(Tag("car"), Tag("bus")),
      mapget.setComment("a comment so fine"),
      mapget.setId("e53")).toMap.result(LazyMap())

    val map1 = MapGet.seq(
      mapget.setTags(Tag("car"), Tag("bus")),
      mapget.setComment("a comment so fine"),
      mapget.setId("e52")).toMap.result(LazyMap())

    val maps = Seq(map0, map1)
    val mf = MetadataFile(f).write(maps)
    val res = mf.find(TrueCondition)
    res should have size (2)
    mf.querySize("comment:*fine*") should be (2)

    val maps2 = MapGet.filter(maps, mapget.setComment("new comment!").map(_ => true))
    val mf2 = mf.write(maps2)
    mf2.find(TrueCondition) should have size (2)
    mf2.querySize("comment:*fine*") should be (0)
    mf2.querySize("comment:*new*") should be (2)
  }

  "find" should "find nothing for non existing file" in withNewFile { f =>
    f.exists should be (false)
    val mf = MetadataFile(f)
    mf.find(TrueCondition) should be ('empty)
  }

  it should "find single tags" in withTags(Tag("swim")) { mf =>
    mf.querySize("tag:swim") should be (1)
    mf.querySize("tag:sw*") should be (1)
    mf.querySize("tag:abc") should be (0)
  }

  it should "find tags in many" in withTags(Tag("swim"), Tag("bold")) { mf =>
    mf.querySize("tag?") should be (1)
    mf.querySize("tag:bold") should be (1)
    mf.querySize("tag:swim") should be (1)
    mf.querySize("tag:abc") should be (0)
    mf.querySize("(| tag:swim tag:abc)") should be (1)
    mf.querySize("(| tag:swim tag:bold)") should be (1)
  }

  it should "search comments" in withComment("a blue brown fox") { mf =>
    mf.querySize("comment:*blue*") should be (1)
    mf.querySize("comment:*fox") should be (1)
    mf.querySize("comment:brown") should be (0)
  }

  it should "return empty for non existing properties" in {
    withTags(Tag("bus")) { mf =>
      mf.querySize("comment?") should be (0)
      mf.querySize("comment:*b*") should be (0)
    }
    withComment("blue brown fox") { mf =>
      mf.querySize("tag:bus") should be (0)
      mf.querySize("tag?") should be (0)
    }
  }

  def withMetadata(comment: String, tags: Tag*)(code: MetadataFile => Any): Unit = withNewFile { f =>
    f.exists should be (false)
    val mf = MetadataFile(f).write(Seq(MapGet.seq(
      mapget.setTags(tags),
      mapget.setComment(comment),
      mapget.setId("e53")).toMap.result(LazyMap())))
    Try(code(mf)) match {
      case Success(_) =>
      case Failure(ex) =>
        println(f.contentAsString)
        throw ex
    }
  }

  def withTags(tags: Tag*)(code: MetadataFile => Any): Unit =
    withMetadata("", tags: _*)(code)

  def withComment(comment: String)(code: MetadataFile => Any): Unit =
    withMetadata(comment)(code)

  implicit class MetadataTestOps(mf: MetadataFile) {
    def querySize(q: String): Int =
      mf.query(q) match {
        case Right(rs) => rs.size
        case Left(msg) => sys.error(msg)
      }
  }
}
