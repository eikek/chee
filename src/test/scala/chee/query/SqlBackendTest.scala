package chee.query

import org.scalatest._
import chee.properties._
import chee.TestInfo

class SqlBackendTest extends FlatSpec with Matchers {
  import SqlBackend._

  "whereClause" should "translate exists to null check" in {
    whereClause(Exists('path)) should be ("path is not null")
    whereClause(Not(Exists('path))) should be ("not(path is not null)")
  }

  it should "traslate identprops" in {
    whereClause(IdentProp(Comp.Gt, Ident.width, Ident.height)) should be (
      "width > height "
    )
  }

  it should "translate true to a tautologic condition" in {
    whereClause(TrueCondition) should be ("1=1")
  }

  it should "translate numbers to standard comparisons" in {
    whereClause(Prop(Comp.Lt, Ident.width -> "33")) should be ("width < 33")
    whereClause(Prop(Comp.Gt, Ident.width -> "33")) should be ("width > 33")
    whereClause(Prop(Comp.Eq, Ident.width -> "33")) should be ("width = 33")
  }

  it should "transalte pixel to width*height" in {
    whereClause(Prop(Comp.Eq, VirtualProperty.defaults.pixel.ident -> "33")) should be ("(width * height) = 33")
  }

  it should "translate like to string comparisons" in {
    whereClause(Prop(Comp.Like, Ident.path -> "/a*")) should be ("path like '/a%'")
    whereClause(Prop(Comp.Like, Ident.width -> "20*")) should be ("width like '20%'")
    whereClause(Prop(Comp.Like, Ident.created -> "2012*")) should be ("created like '2012%'")
    whereClause(Prop(Comp.Like, Ident.lastModified -> "2012*")) should be ("datetime(lastmodified/1000, 'unixepoch') like '2012%'")
    whereClause(Prop(Comp.Like, Ident.added -> "2012*")) should be ("datetime(added/1000, 'unixepoch') like '2012%'")
  }

  it should "translate to correct and/or" in {
    val p = Prop(Comp.Lt, Ident.width -> "3000")
    whereClause(Condition.and(p, p, p)) should be ("(width < 3000 and width < 3000 and width < 3000)")
    whereClause(Condition.or(p, p, p)) should be ("(width < 3000 or width < 3000 or width < 3000)")
    whereClause(Condition.and(p, p, Condition.or(p, p))) should be (
      "(width < 3000 and width < 3000 and (width < 3000 or width < 3000))"
    )
  }

  it should "parse created values and compare numerically when not using like" in {
    val p1 = Prop(Comp.Gt, Ident.created -> "-7d")
    val p2 = Prop(Comp.Gt, Ident.created -> "2015-02")
    whereClause(p1) should startWith ("datetime(created) > ")
    whereClause(p1).substring(19) should fullyMatch regex (" datetime\\([0-9]{10}, 'unixepoch'\\)".r)
    whereClause(p2) should be ("datetime(created) > datetime(1422745200, 'unixepoch')")
  }

  it should "use created string as is when using like" in {
    val p1 = Prop(Comp.Like, Ident.created -> "2015*")
    whereClause(p1) should be ("created like '2015%'")
  }

  it should "convert lastmodified timestamp into datetime when using like" in {
    val p1 = Prop(Comp.Like, Ident.lastModified -> "2015*")
    val p2 = Prop(Comp.Like, Ident.added -> "2015*")
    whereClause(p1) should be ("datetime(lastmodified/1000, 'unixepoch') like '2015%'")
    whereClause(p2) should be ("datetime(added/1000, 'unixepoch') like '2015%'")
  }

  it should "compare lastmodified value against parsed datetime when not using like" in {
    val p1 = Prop(Comp.Gt, Ident.lastModified -> "2015-02")
    val p2 = Prop(Comp.Gt, Ident.added -> "2015-02")
    whereClause(p1) should be ("lastmodified > 1422745200000")
    whereClause(p2) should be ("added > 1422745200000")
  }

  "updateRow statement" should "should not update added property" in {
    val image = chee.TestInfo.images.find(_.name == "CIMG2590_s.JPG").get
    val map = LazyMap.fromFile(image) + (Ident.location -> "./") + Extraction.added(DateTime.now)
    val Some(lastmod) = MapGet.value(Ident.lastModified).result(map)
    val Some(path) = MapGet.value(Ident.path).result(map)

    SqlBackend.updateRowStatement("chee_index").result(map) should be (
      "UPDATE chee_index SET " +
        s"path = '$path', "+
        s"filename = 'CIMG2590_s.JPG', length = 48746, lastmodified = $lastmod, " +
        "mimetype = 'image/jpeg', extension = 'JPG', "+
        "checksum = '95254d11a2916bff2357ea3f1572d366398de17150b8ef11d5f11ef8061f371b', "+
        "location = './', make = 'CASIO COMPUTER CO.,LTD', "+
        "model = 'EX-Z750', width = 100, height = 75, iso = null, " +
        "orientation = 1, created = '2012-11-26 13:50:19' " +
        s"WHERE path = '$path'")
  }
}
