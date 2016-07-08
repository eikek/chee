package chee.cli

import chee.{FileLoan, TestInfo, UserError}
import chee.it._
import chee.util.files._
import com.typesafe.scalalogging.LazyLogging
import org.scalatest._

class MoveTest extends FlatSpec with Matchers with CommandSetup with FindHelper with LazyLogging with FileLoan {

  def move = new Move with BufferOut
  def linfo = new Info with BufferOut

  "Move" should "error if source dir is not a location" in bothChee(addImages) { setup =>
    val nonExistingDir = setup.userDir / "does-not-exist"
    intercept[UserError] {
      move.run(setup, nonExistingDir.pathAsString, nonExistingDir.sibling("targetdir").pathAsString)
    }
  }

  it should "rename a location" in bothChee(addImages) { setup =>
    val images = setup.userDir / "images"
    val (stdout, Nil) = move.run(setup, setup.files.pathAsString, images.pathAsString)
    stdout should have size (2)

    // check if they can be found with new path
    val (result, Nil) = find.run(setup, "-a", "-p", "paths", "path:*images*")
    val imagesFiles = images.list.toList.map(_.pathAsString).sorted
    imagesFiles should not be empty
    result.filter(_.nonEmpty).sorted should be (imagesFiles)

    // check if new location shows up
    val (info, Nil) = linfo.run(setup)
    info(0) should startWith (images.pathAsString)
  }

  it should "move multiple files in another directory" in bothChee(addImages) { setup =>
    val dir = setup.files / "bla"
    dir.createDirectories()
    val src = setup.files.list.filter(_.isRegularFile).take(2).toList
    src should have size (2)

    val (beforeOut, Nil) = findLisp(setup, src.map(s => s"path=${s.path}").mkString("(| ", " ", ")"))
    beforeOut should have size (2)
    val (beforeLoc, Nil) = linfo.run(setup)

    val (out, Nil) = move.run(setup, (src :+ dir).map(_.pathAsString): _*)
    out should have size (3)

    val (fout, Nil) = findLisp(setup, s"""path:${dir.pathAsString}*""")
    fout should have size (2)
    // only path propery has changed
    beforeOut.map(s => s.replace(
      s""":path "${setup.files.path}""",
      s""":path "${dir.path}""")) should be (fout)

    // location list should not change
    val (afterLoc, Nil) = linfo.run(setup)
    beforeLoc should be (afterLoc)
  }

  it should "refuse to move multiple files in non existing target" in bothChee(addImages) { setup =>
    val dir = setup.files / "bla"
    val src = setup.files.list.filter(_.isRegularFile).take(2).toList
    src should have size (2)

    intercept[UserError] {
      move.run(setup, (src :+ dir).map(_.pathAsString): _*)
    }  
  }

  it should "move files from one location to another" in bothChee(addImages) { setup =>
    val otherLoc = setup.userDir / "morefiles"
    addLocation(otherLoc, setup)

    val src = setup.files.list.filter(_.isRegularFile).take(2).toList
    src should have size (2)

    val (beforeOut, Nil) = findLisp(setup, src.map(s => s"path=${s.path}").mkString("(| ", " ", ")"))
    beforeOut should have size (2)
    val (beforeLoc, Nil) = linfo.run(setup)
    beforeLoc should have size(3)

    val dir = (otherLoc / "newstuff")
    dir.createDirectories()

    val (mvOut, Nil) = move.run(setup, (src :+ dir).map(_.pathAsString): _*)
    mvOut should have size (3)

    val (fout, Nil) = findLisp(setup, s"""path:${dir.pathAsString}*""")
    fout should have size (2)

    // path and location propery has changed
    beforeOut.map(s => s
      .replace(
        s""":path "${setup.files.path}""",
        s""":path "${dir.path}""")
      .replace(
        s""":location "${setup.files.path}"""",
        s""":location "${otherLoc.path}"""")) should be (fout)

    // location list should change
    val (afterLoc, Nil) = linfo.run(setup)
    beforeLoc should have size (afterLoc.size)
  }

  it should "rename files" in bothChee(addImages) { setup =>
    val src = setup.files.list.toStream
      .filter(_.getExtension.map(_.toLowerCase) == Some("jpg"))
      .headOption.getOrElse(sys.error("invalid test setup"))

    val (beforeOut, Nil) = findLisp(setup, s"filename=${src.name}")
    beforeOut should have size (1)
    val (beforeLoc, Nil) = linfo.run(setup)
    beforeLoc should have size(2)

    val (out, err) = move.run(
      setup,
      src.pathAsString,
      (setup.files / "tada.jpg").pathAsString)

    val (afterOut, Nil) = findLisp(setup, s"filename=tada.jpg")
    afterOut should have size (1)

    // path, filename and extension changed
    beforeOut.map(s => s
      .replace(
        s""":path "${src.path}""",
        s""":path "${(setup.files / "tada.jpg").path}""")
      .replace(
        s""":filename "${src.name}"""",
        s""":filename "tada.jpg"""")
      .replace(
        s""":extension "JPG"""",
        s""":extension "jpg"""")) should be (afterOut)
  }

  it should "rename directories" in bothChee(addImages) { setup =>
    val subdir = setup.files / "dir1"
    subdir.createDirectories()

    val target = setup.files / "dir2"

    // first move files in a subdir
    val files = setup.files.list.filterNot(_ == subdir).map(_.pathAsString).toSeq 
    val (_, Nil) = move.run(setup, (files :+ subdir.pathAsString): _*)

    val (beforeOut, Nil) = findLisp(setup)
    beforeOut should have size (4)
    val (beforeLoc, Nil) = linfo.run(setup)

    // rename
    val (out, Nil) = move.run(setup,
      subdir.pathAsString,
      target.pathAsString)
    out should have size (2)

    val (afterOut, Nil) = findLisp(setup)
    afterOut should have size (4)

    beforeOut.map(s => s
      .replace(
        s""":path "${subdir.path}""",
        s""":path "${target.path}""")) should be (afterOut)

    // location list should not change
    val (afterLoc, Nil) = linfo.run(setup)
    beforeLoc should be (afterLoc)
  }

  it should "refuse if source does not exist" in bothChee(addImages) { setup =>
    intercept[UserError] {
      move.run(setup,
        (setup.files / "234.png").pathAsString,
        (setup.files / "456.png").pathAsString)
    }
  }

  it should "refuse if target is child of source" in bothChee(addImages) { setup =>
    intercept[UserError] {
      move.run(setup,
        setup.files.pathAsString,
        (setup.files / "foo").pathAsString)
    }
  }

  it should "refuse if target exists" in bothChee(addImages) { setup =>
    intercept[UserError] {
      move.run(setup,
        setup.files.list.toList(0).pathAsString,
        setup.files.list.toList(1).pathAsString)
    }
  }

  it should "move a location into another" in bothChee(addImages) { setup =>
    val otherLoc = setup.userDir / "morefiles"
    addLocation(otherLoc, setup)

    val (beforeOut, Nil) = findLisp(setup)
    val (beforeLoc, Nil) = linfo.run(setup)
    
    val (out, Nil) = move.run(setup,
      otherLoc.pathAsString,
      setup.files.pathAsString)

    val (afterOut, Nil) = findLisp(setup)
    val (afterLoc, Nil) = linfo.run(setup)

    (setup.files / otherLoc.name).exists should be (true)

    // path and location propery has changed
    beforeOut.map(s => s
      .replace(
        s""":path "${otherLoc.path}""",
        s""":path "${(setup.files / otherLoc.name).path}""")
      .replace(
        s""":location "${otherLoc.path}"""",
        s""":location "${setup.files.path}"""")) should be (afterOut)

    // location list should not change, but its counts
    beforeLoc.map(s => s
      .replace(
        "/files: 4",
        "/files: 8")
      .replace(
        "/morefiles: 4",
        "/morefiles: 0")).filterNot(_ endsWith "0") should be (afterLoc)
  }

  it should "move a directory back to a location root" in bothChee(addImages) { setup =>
    val otherLoc = setup.userDir / "morefiles"
    addLocation(otherLoc, setup)
    val (beforeOut, Nil) = findLisp(setup)
    val (beforeLoc, Nil) = linfo.run(setup)
    val (_, Nil) = move.run(setup,
      otherLoc.pathAsString,
      setup.files.pathAsString)
    otherLoc.exists should be (false)


    val (_, Nil) = move.run(setup,
      (setup.files / otherLoc.name).pathAsString,
      otherLoc.pathAsString)

    val (afterOut, Nil) = findLisp(setup)
    val (afterLoc, Nil) = linfo.run(setup)

    afterOut should be (beforeOut)
    afterLoc should be (beforeLoc)
  }

  it should "not move files with --index" in bothChee(addImages) { setup =>
    val images = setup.userDir / "images"
    val (stdout, Nil) = move.run(setup, "--index", setup.files.pathAsString, images.pathAsString)
    stdout should have size (2)
    setup.files.exists should be (true)
    images.exists should be (false)
    val (out, Nil) = findLisp(setup, "path:*images*")
    out should have size (4)
  }

  it should "refuse to move non-indexed files" in bothChee() { setup =>
    val target = (setup.files / "repo").createDirectories()
    copyFile(TestInfo.images(2), Some(target)) { file =>
      val newName = file.mapBaseName(_ => "funny")
      intercept[UserError] {
        move.run(setup, file.pathAsString, newName.pathAsString)
      }
    }
  }
}
