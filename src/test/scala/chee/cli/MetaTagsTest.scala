package chee.cli

import chee.it.{BufferOut, CommandSetup, FindHelper}
import org.scalatest._

class MetaTagsTest extends FlatSpec with Matchers with CommandSetup with FindHelper {

  def attach = new MetaAttach with BufferOut
  def tagscmd = new MetaTags with BufferOut

  "tags" should "list all tags with its count" in bothChee(addImages) { setup =>
    val (_, Nil) = attach.run(setup, "--tags", "eagle,sheep")
    val (out, Nil) = tagscmd.run(setup)
    out should have size (2)
    out(0) should be ("eagle: 5")
    out(1) should be ("sheep: 5")
  }

}
