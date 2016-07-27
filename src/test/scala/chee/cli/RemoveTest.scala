package chee.cli

import chee.{TestInfo, UserError}
import chee.it._
import com.typesafe.scalalogging.LazyLogging
import org.scalatest._

class RemoveTest extends FlatSpec with Matchers with CommandSetup with FindHelper with LazyLogging {

  def move = new Move with BufferOut
  def remove = new Remove with BufferOut
  def linfo = new Info with BufferOut

  "Remove" should "error if source dir is not a location" in bothChee(addImages) { setup =>
    val nonExistingDir = setup.userDir / "does-not-exist"
    intercept[UserError] {
      remove.run(setup, nonExistingDir.pathAsString)
    }
  }

  it should "remove existing files" in bothChee(addImages) { setup =>
    val (before, Nil) = findLisp(setup)
    val (beforeLoc, Nil) = linfo.run(setup)

    val (_, Nil) = remove.run(setup, setup.files.list.take(2).map(_.pathAsString).toSeq: _*)

    val (after, Nil) = findLisp(setup)
    val (afterLoc, Nil) = linfo.run(setup)
    before.size - 2 should be (after.size)
    beforeLoc.map(_.replace(s": ${TestInfo.images.size}", s": ${TestInfo.images.size -2}")) should be (afterLoc)
  }

  it should "remove directories recursively" in bothChee(addImages) { setup =>
    val subdir = setup.files / "dir1"
    subdir.createDirectories()
    // first move files in a subdir
    val files = setup.files.list.filterNot(_ == subdir).take(2).map(_.pathAsString).toSeq
    val (_, Nil) = move.run(setup, (files :+ subdir.pathAsString): _*)

    val (before, Nil) = findLisp(setup)
    val (beforeLoc, Nil) = linfo.run(setup)

    val (_, Nil) = remove.run(setup, subdir.pathAsString)

    val (after, Nil) = findLisp(setup)
    val (afterLoc, Nil) = linfo.run(setup)

    before.size - 2 should be (after.size)
    beforeLoc.map(_.replace(s": ${TestInfo.images.size}", s": ${TestInfo.images.size -2}")) should be (afterLoc)
  }

  it should "remove a location root" in bothChee(addImages) { setup =>
    val (before, Nil) = findLisp(setup)
    val (beforeLoc, Nil) = linfo.run(setup)
    before.size should be > (1)
    beforeLoc.size should be > 1

    val (_, Nil) = remove.run(setup, setup.files.pathAsString)

    val (after, Nil) = findLisp(setup)
    val (afterLoc, Nil) = linfo.run(setup)
    after should be ('empty)
    afterLoc should have size (1)
    afterLoc(0) should be ("All: 0")
    setup.files.exists should be (false)
  }

  it should "not delete files with --index" in bothChee(addImages) { setup =>
    val (before, Nil) = findLisp(setup)
    val (beforeLoc, Nil) = linfo.run(setup)
    before.size should be > (1)
    beforeLoc.size should be > 1

    val (_, Nil) = remove.run(setup, "--index", setup.files.pathAsString)

    val (after, Nil) = findLisp(setup)
    val (afterLoc, Nil) = linfo.run(setup)
    after should be ('empty)
    afterLoc should have size (1)
    afterLoc(0) should be ("All: 0")
    setup.files.exists should be (true)
  }
}
