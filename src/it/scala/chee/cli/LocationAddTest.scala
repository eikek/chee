package chee.cli

import chee.TestInfo
import chee.it.{ BufferOut, CommandSetup, FindHelper }
import org.scalatest._

class LocationAddTest extends FlatSpec with Matchers with CommandSetup with FindHelper {

  def attach = new MetaAttach with BufferOut
  def metaFind = new MetaFind with BufferOut
  val addCmd = new LocationAdd with BufferOut

  "location add" should "add files that have metadata" in bothChee() { setup =>
    setup.files.createIfNotExists(asDirectory = true)
    TestInfo.images.foreach(f => f.copyTo(setup.files / f.name))

    val (_, Nil) = attach.run(setup, "-f", setup.files.path.toString, "--tags", "bike,car")
    val (out0, Nil) = metaFind.run(setup, "-p", "~:checksum~%", "checksum?")
    out0.size should be > (0)

    val (_, Nil) = addCmd.run(setup, "-r", setup.files.path.toString)
    val (out1, Nil) = findLisp(setup)
    out1.size should be (out0.size)
  }

}
