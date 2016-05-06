package chee.metadata

import chee.FileLoan
import chee.properties.{ FormatPatterns, LazyMap, MapGet, TrueCondition }
import org.scalatest.{ FlatSpec, Matchers }

class MetadataFileTest extends FlatSpec with Matchers with FileLoan {

  "write" should "create a new file" in withNewFile { f =>
    f.exists should be (false)

    val map = MapGet.seq(
      mapget.setTags(Tag("car"), Tag("bus")),
      mapget.setComment("a comment so fine"),
      mapget.setId("e53")).toMap.result(LazyMap())

    val mf = MetadataFile(f).write(Stream(map))
    val res = mf.find(TrueCondition)
    res should have size (1)
    FormatPatterns.lisp.result(res.head) should be (FormatPatterns.lisp.result(map))
  }

}
