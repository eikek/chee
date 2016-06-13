package chee.cli

import org.scalatest._
import chee.it._

class CollectionTest extends FlatSpec with Matchers with CommandSetup {

  def collRemove = new CollectionRemove with BufferOut
  def collEdit = new CollectionEdit with BufferOut
  def collShow = new CollectionShow with BufferOut

  "collection add" should "add new collections" in bothChee() { setup =>
    collEdit.run(setup, "--query", "mime:image/*", "images")
    val (out, Nil) = collShow.run(setup)
    out should have size (1)
    out(0) should be ("images - ")
  }

  "collection remove" should "remove existing collections" in bothChee() { setup =>
    collEdit.run(setup, "--query", "mime:image/*", "images")
    collEdit.run(setup, "--query", "mime:video/*", "videos")
    val (out1, Nil) = collShow.run(setup)
    out1 should have size (2)
    out1(0) should be ("images - ")
    out1(1) should be ("videos - ")

    val (_, Nil) = collRemove.run(setup, "images")
    val (out2, Nil) = collShow.run(setup)
    out2 should have size (1)
    out2(0) should be ("videos - ")
  }

  it should "refuse for unknown names" in bothChee() { setup =>
    val (Nil, err) = collRemove.run(setup, "videos")
    err should have size (1)
  }

  it should "find names by prefix" in bothChee() { setup =>
    collEdit.run(setup, "--query", "mime:image/*", "images")
    val (_, Nil) = collRemove.run(setup, "im")
    val (Nil, Nil) = collShow.run(setup)
  }
}
