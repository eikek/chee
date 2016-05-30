package chee.properties

import chee.FileLoan
import org.scalatest._
import PropertyMap._
import Ident._
import chee.TestInfo

class PropertyMapTest extends FlatSpec with Matchers with FileLoan {

  val p1 = Property(filename, "test")
  val p2 = Property(filename, "tset")

  val p3 = Property(make, "Nikon")
  val p4 = Property(model, "D200")
  val p5 = Property(Ident.length, "123423")

  val Empty = PropertyMap.empty

  def image(name: String) = TestInfo.images.find(_.name == name).getOrElse(sys.error(s"image '$name' not found"))

  "add" should "replace existing values" in {
    (PropertyMap(p1) + p2) should be (PropertyMap(p2))
  }

  it should "add new elements" in {
    val map = PropertyMap(p1, p3)
    (map + p4) should be (PropertyMap(p1, p3, p4))
    (map ++ PropertyMap(p1, p4)) should be (PropertyMap(p1, p3, p4))
    (Empty + p1) should be (PropertyMap(p1))
    (Empty ++ PropertyMap(p1)) should be (PropertyMap(p1))
  }

  "empty ++ empty" should "be empty" in {
    (Empty ++ Empty) should be (Empty)
  }

  "create" should "make empty and nonempty maps" in {
    PropertyMap() should be (Empty)
    PropertyMap(p1) should not be (Empty)
    PropertyMap(p1, p2, p3) should be (PropertyMap(p2, p3))
    fromOptions(None, None, None) should be (Empty)
    fromOptions(None, Some(p1), Some(p2)) should be (PropertyMap(p2))
  }

  "LazyMap" should "get file checksum" in {
    val m = LazyMap.fromFile(image("test1.jpg"))
    val (next, Some(check)) = MapGet.value(Ident.checksum).run(m)
    check should be ("b6bdc5b62c489ebfa55738fb41a88133cdd52ee0785baf4ccdcc26bcd62a736e")
  }

  it should "get file name" in {
    val m = LazyMap.fromFile(image("test1.jpg"))
    val (next, Some(check)) = MapGet.value(Ident.filename).run(m)
    check should be ("test1.jpg")
  }

  it should "get image width" in {
    val m = LazyMap.fromFile(image("test1.jpg"))
    val (next, Some(check)) = MapGet.value(Ident.width).run(m)
    check should be ("1900")
  }

  it should "support virtual properties" in {
    val m = LazyMap.fromFile(image("test1.jpg")).addVirtual(VirtualProperty.defaults.pixel)
    val (next, Some(px)) = MapGet.value('pixel).run(m)
    px should be ((1900 * 1267).toString)
  }

  it should "append two maps" in {
    val m1 = LazyMap(Ident.filename -> "test.jpg")
    val m2 = LazyMap(Ident.path -> "/a/b/test.jpg")
    MapGet.value(Ident.path).result(m1 ++ m2) should be (Some("/a/b/test.jpg"))
    MapGet.value(Ident.filename).result(m1 ++ m2) should be (Some("test.jpg"))
  }

  it should "prefer first map when appending" in {
    val m1 = LazyMap(Ident.filename -> "bird.jpg")
    val m2 = LazyMap(Ident.filename -> "test.jpg")
    MapGet.value(Ident.filename).result(m1 ++ m2) should be (Some("bird.jpg"))
  }

  it should "not cache virtual property values" in {
    val m1 = LazyMap.fromFile(image("test1.jpg"))
    val (m2, pixel) = MapGet.valueForce(VirtualProperty.idents.pixel).run(m1)
    pixel should be ("2407300")
    val m3 = m2 + (Ident.width -> "100") + (Ident.height -> "100")
    MapGet.valueForce(VirtualProperty.idents.pixel).result(m3) should be ("10000")
  }

  it should "apply mapIdents" in {
    val m1 = LazyMap.fromFile(image("CIMG2590_s.JPG")).mapIdents(id => id.in("old"))
    m1.getVirtual(Ident("old-pixel")) should not be (None)
    MapGet.valueForce(Ident("old-height")).result(m1) should be ("75")
    MapGet.valueForce(Ident("old-width")).result(m1) should be ("100")

    MapGet.value(VirtualProperty.idents.pixel).result(m1) should be (None)
    MapGet.valueForce(Ident("old-pixel")).result(m1) should be ("7500")
  }

  it should "not overwrite existing properties by extractors" in {
    val m0 = LazyMap.fromFile(image("test1.jpg"))
    val m1 = m0 + (Ident.filename -> "myname.txt")
    MapGet.valueForce(Ident.filename).result(m1) should be ("myname.txt")

    // call to get length requires file extractor
    val (m2, ext) = MapGet.valueForce(Ident.length).run(m1)
    ext.toInt should be > (0)

    // the extractor should not overwrite the filename
    MapGet.valueForce(Ident.filename).result(m2) should be ("myname.txt")
  }

  it should "not reinsert missing properties from extractors" in  {
    val m0 = LazyMap.fromFile(image("test1.jpg"))
    val m1 = m0 + (Ident.filename -> "myname.txt")
    MapGet.valueForce(Ident.filename).result(m1) should be ("myname.txt")

    // now explicitely remove filename
    val m2 = m1 remove Ident.filename
    MapGet.value(Ident.filename).result(m2) should be ('empty)

    // call to get length requires file extractor
    val (m3, ext) = MapGet.valueForce(Ident.length).run(m2)
    ext.toInt should be > (0)

    // the extractor should not reinsert the filename
    MapGet.value(Ident.filename).result(m3) should be ('empty)
  }
}
