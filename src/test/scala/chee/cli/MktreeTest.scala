package chee.cli

import better.files.File
import chee.{FileLoan, TestInfo}
import chee.FileOps.Result.Skipped
import chee.it.{BufferOut, CommandSetup, FindHelper}
import chee.properties.{Ident, ImageExtract}
import chee.query.FileBackend
import org.scalatest._

class MkTreeTest extends FlatSpec with Matchers with CommandSetup with FindHelper with FileLoan with chee.MoreMatcher {

  def addCmd = new Add with BufferOut
  def mktree = new MkTree with BufferOut
  def move = new Move with BufferOut
  def sync = new Sync with BufferOut

  def checkDefaultPattern(dir: File, check: File => Any): Unit = {
    val extract = new ImageExtract()
    TestInfo.images.foreach { img =>
      extract.extractMetadata(img).get(Ident.created) match {
        case Some(created) =>
          val year = created.substring(0, 4)
          val month = created.substring(5, 7)
          (dir/year/month) should exist

          val day = created.substring(8, 10)
          val hour = created.substring(11, 13)
          val min = created.substring(14, 16)
          val f = dir/year/month/(s"$day-$hour-${min}_${img.name}")
          f should exist
          check(f)
        case _ =>
          img.name should endWith ("png")
      }
    }
  }

  "mktree" should "symlink files with default pattern" in bothChee(addImages) { setup =>
    newDirectory { dir =>
      val (out, err) = mktree.run(setup, "--target", dir.pathAsString, "len>0")
      err should be ('empty)
      checkDefaultPattern(dir, _.isSymbolicLink should be (true))
    }
  }

  it should "copy files with default pattern" in bothChee(addImages) { setup =>
    newDirectory { dir =>
      val (out, err) = mktree.run(setup, "--copy", "--target", dir.pathAsString, "len>0")
      err should be ('empty)
      checkDefaultPattern(dir, _.isRegularFile should be (true))
    }
  }

  it should "fix dangling links" in bothChee(addImages) { setup =>
    val img = setup.files / TestInfo.images.randomGet.name
    val target = setup.files / "new_folder"
    target.createDirectories()
    newDirectory { dir =>
      mktree.run(setup, "--target", dir.pathAsString, "len>0")
      // now move original files to make stale links
      val (_, err) = move.run(setup, img.pathAsString, target.pathAsString)
      err should be ('empty)

      // running mktree again creates the same target tree
      val (_, err2) = mktree.run(setup, "--target", dir.pathAsString, "len>0")
      err2 should be ('empty)
      checkDefaultPattern(dir, _.isSymbolicLink should be (true))
      val count = dir.listRecursively.foldLeft(0) {(c, f) =>
        f should exist
        if (!f.isDirectory) c+1 else c
      }
      count should be (TestInfo.images.size)
    }
  }

  def fileSizes(dir: File): Map[File, Long] =
    FileBackend.walk(dir, true)
      .filter(f => !f.isDirectory)
      .map(f => (f, f.size))
      .toMap

  it should "overwrite existing files" in bothChee(addImages) { setup =>
    newDirectory { dir =>
      mktree.run(setup, "--copy", "--pattern", "~:file", "--target", dir.pathAsString, "len>0")
      val sizes = fileSizes(dir)
      sizes should be ('nonEmpty)
      //rename files such that sizes differ
      val images = TestInfo.images.map(_.name)
      setup.files.list.foreach { f => f.renameTo(images.indexOf(f.name).toString) }
      val imgperm = images.tail ::: List(images.head)
      setup.files.list.foreach { f =>
        val newname = imgperm(f.name.toInt)
        f.renameTo(newname)
      }
      sync.run(setup, setup.files.pathAsString)

      // run mktree again that should create the same tree but with renamed files
      mktree.run(setup, "--copy", "--overwrite", "--pattern", "~:file", "--target", dir.pathAsString, "len>0")
      val files = FileBackend.walk(dir, true).filter(f => !f.isDirectory).toList
      files should not be ('empty)
      files.foreach { f =>
        f.size should not be (sizes(f))
      }
    }
  }

  it should "skip existing files" in bothChee(addImages) { setup =>
    newDirectory { dir =>
      mktree.run(setup, "--copy", "--pattern", "~:file", "--target", dir.pathAsString, "len>0")
      val sizes = fileSizes(dir)
      sizes should be ('nonEmpty)
      val (out, _) = mktree.run(setup, "--copy", "--pattern", "~:file", "--target", dir.pathAsString, "len>0")
      val files = FileBackend.walk(dir, true).filter(f => !f.isDirectory).toList
      files should not be ('empty)
      files.foreach { f => f.size should be (sizes(f)) }
      out.init.forall(s => s contains Skipped.toString) should be (true)
    }
  }
}
