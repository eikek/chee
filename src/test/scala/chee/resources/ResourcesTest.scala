package chee.resources

import org.scalatest._
import java.net.URL

class ResourcesTest extends FlatSpec with Matchers {

  "resources" should "be in classpath" in {
    for {
      cat <- ResourceInfo.categories
      (_, v) <- ResourceInfo.allOf(cat)
    } v.forall(checkContent) should be (true)

    checkContent(ResourceInfo.galleryTemplate) should be (true)

    for (cat <- ResourceInfo.categories) {
      val kinds = (for ((k, _) <- ResourceInfo.allOf(cat)) yield k).toSet
      (cat -> kinds) should be (cat -> Set('js, 'css, 'img, 'fonts))
    }
  }

  def checkContent(url: URL) = {
    val conn = url.openConnection
    conn.connect
    val in = conn.getInputStream
    in.read should not be (-1)
    in.close
    true
  }
}
