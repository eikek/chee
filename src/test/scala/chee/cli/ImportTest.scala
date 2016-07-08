package chee.cli

import chee.{FileLoan, TestInfo, UserError}
import chee.it.{ BufferOut, CommandSetup, FindHelper }
import org.scalatest._

class ImportTest extends FlatSpec with Matchers with CommandSetup with FindHelper with FileLoan {

  def attach = new MetaAttach with BufferOut
  def metaFind = new MetaFind with BufferOut
  def addCmd = new Add with BufferOut
  def imp = new Import with BufferOut
  val pass = "test".toCharArray

  "import" should "respect repository root" in repoChee() { setup =>
    intercept[UserError] {
      newDirectory { target =>
        imp.run(setup, TestInfo.images(0).parent.pathAsString, target.pathAsString)
      }
    }
  }

  it should "import files" in bothChee() { setup =>
    val target = setup.files.createDirectories()

    val (_, Nil) = imp.run(setup, TestInfo.images(0).pathAsString, target.pathAsString)
    val (loc, Nil) = find.run(setup, "-p", "~#location~%")
    loc should have size (1)
    loc(0) should be (target.pathAsString)

    val (path, Nil) = find.run(setup, "-p", "paths")
    path(0) should be ((target / TestInfo.images(0).name).pathAsString)
  }

  it should "import directories" in bothChee() { setup =>
    val target = setup.files.createDirectories()

    val (_, Nil) = imp.run(setup, "-r", TestInfo.images(0).parent.parent.pathAsString, target.pathAsString)
    val (out, Nil) = find.run(setup, "-p", "paths")
    out.map(_.substring((target / "images").pathAsString.length +1)).sorted should be (
      TestInfo.images.map(_.name).sorted
    )
  }

  it should "normalize locations" in bothChee() { setup =>
    val outer = setup.files / "outer"
    val inner = outer / "inner"
    inner.createDirectories()

    //first import to inner
    val (_, Nil) = imp.run(setup, TestInfo.images(0).pathAsString, inner.pathAsString)
    //then in outer
    val (_, Nil) = imp.run(setup, TestInfo.images(1).pathAsString, outer.pathAsString)

    // locations should all be outer
    val (out, Nil) = find.run(setup, "-p", "~#location~%")
    out should have size (2)
    out.toSet should have size (1)
    out(0) should be (outer.pathAsString)
  }

  it should "import encrypted files" in bothChee() { setup =>
    val target = (setup.files / "repo").createDirectories()
    val source = (setup.files / "src").createDirectories()
    withNewFile { passfile =>
      passfile `<<` String.valueOf(pass)
      encryptFile(TestInfo.images(2), pass, Some(source)) { enc =>
        val (_, Nil) = imp.run(setup, "-r", "-d", "--method", "password", "--passphrase", passfile.pathAsString, source.pathAsString, target.pathAsString)
      }
      val (out, Nil) = find.run(setup, "-p", "~:width~:height~%")
      out should have size (1)
      out(0) should fullyMatch regex ("[0-9]+")
    }
  }
}
