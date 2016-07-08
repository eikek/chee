package chee.cli

import chee.properties.BasicExtract
import chee.util.files._
import chee.{FileLoan, TestInfo, UserError}
import chee.it.{ BufferOut, CommandSetup, FindHelper }
import org.scalatest._

class AddTest extends FlatSpec with Matchers with CommandSetup with FindHelper with FileLoan {

  def attach = new MetaAttach with BufferOut
  def metaFind = new MetaFind with BufferOut
  def addCmd = new Add with BufferOut

  "add" should "add files that have metadata" in bothChee() { setup =>
    setup.files.createIfNotExists(asDirectory = true)
    TestInfo.images.foreach(f => f.copyTo(setup.files / f.name))

    val (_, Nil) = attach.run(setup, "-f", setup.files.path.toString, "--tags", "bike,car")
    val (out0, Nil) = metaFind.run(setup, "-p", "~:checksum~%", "checksum?")
    out0.size should be > (0)

    val (_, Nil) = addCmd.run(setup, "-r", setup.files.path.toString)
    val (out1, Nil) = findLisp(setup)
    out1.size should be (out0.size)
  }

  it should "respect repository root" in repoChee() { setup =>
    intercept[UserError] {
      addCmd.run(setup, TestInfo.images(0).parent.pathAsString)
    }
  }

  it should "add encrypted files" in bothChee() { setup =>
    val target = (setup.files / "repo").createDirectories()
    val file = target / TestInfo.images(2).name
    TestInfo.images(2).copyTo(file)
    val contentType = new BasicExtract().mimeType(TestInfo.images(2)).map(_.value).getOrElse("")

    encryptFile(file, "test".toCharArray) { enc =>
      file.delete()
      withNewFile { passfile =>
        passfile `<<` "test"
        val (_, Nil) = addCmd.run(setup, "-d", "--method", "password", "--passphrase", passfile.pathAsString, target.pathAsString)
        val (out, Nil) = findLisp(setup)
        out should have size (1)
        out(0) should include (s""":checksum "${TestInfo.images(2).sha256.toLowerCase}"""")
        out(0) should include (s""":path "${enc.pathAsString}"""")
        out(0) should include (s""":location "${target.pathAsString}"""")
        out(0) should include (s""":filename "${TestInfo.images(2).name}"""")
        out(0) should include (s":length ${TestInfo.images(2).size}")
        out(0) should include (s""":mimetype "${contentType}"""")
        out(0) should include (s""":extension "${TestInfo.images(2).getExtension.get}"""")
      }
    }
  }

  it should "skip existing files" in bothChee() { setup =>
    val target = (setup.files / "repo").createDirectories()
    val file = target / TestInfo.images(2).name
    TestInfo.images(2).copyTo(file)

    val (out1, Nil) = addCmd.run(setup, target.pathAsString)
    out1.mkString("\n") should include ("Added")
    val (list, Nil) = findLisp(setup)

    val (out2, Nil) = addCmd.run(setup, target.pathAsString)
    out2.mkString("\n") should include ("Skipped")
    list should be (findLisp(setup)._1)
  }
}
