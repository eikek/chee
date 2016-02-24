package chee.properties

import org.scalatest._

class FileSizeTest extends FlatSpec with Matchers {

  val conv = FileSizeConverter.fileSize

  "render" should "keep bytes if too small" in {
    conv.render(10) should be ("10b")
    conv.render(210) should be ("210b")
  }

  it should "make kb" in {
    conv.render(12 * 1024) should be ("12kb")
    conv.render((2.1 * 1024).toLong) should be ("2.1kb")
    conv.render((842.2753 * 1024).toLong) should be ("842.3kb")
  }

  it should "make mb" in {
    conv.render(12 * 1024 * 1024) should be ("12mb")
    conv.render((2.1 * 1024 * 1024).toLong) should be ("2.1mb")
    conv.render((842.2753 * 1024 * 1024).toLong) should be ("842.3mb")
  }

  "parse" should "parse bytes" in {
    conv.parse("1231") should be (Right(1231L))
    conv.parse("1231b") should be (Right(1231L))
  }

  it should "find kb" in {
    for (x <- List("12kb", "12.0kb", "12k", "12.0k", "12Kb", "12.0KB")) {
      conv.parse(x) should be (Right(12L * 1024))
    }
  }

  it should "find mb" in {
    for (x <- List("12mb", "12.0mb", "12m", "12.0m", "12Mb", "12.0MB")) {
      conv.parse(x) should be (Right(12L * 1024 * 1024))
    }
  }

  it should "reject invalid strings" in {
    conv.parse("1.0.0mb") should be ('left)
    conv.parse("1.amb") should be ('left)
    conv.parse("1.0.0mbb") should be ('left)
    conv.parse("mb") should be ('left)
    conv.parse("1.0.0mb") should be ('left)
  }
}
