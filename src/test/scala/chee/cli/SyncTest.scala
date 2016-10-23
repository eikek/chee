package chee.cli

import chee.crypto.CheeCrypt
import chee.{FileLoan, TestInfo, UserError}
import chee.it.{ BufferOut, CommandSetup, FindHelper }
import org.scalatest._

class SyncTest extends FlatSpec with Matchers with CommandSetup with FindHelper with FileLoan {

  def attach = new MetaAttach with BufferOut
  def metaFind = new MetaFind with BufferOut
  def addCmd = new Add with BufferOut
  def sync = new Sync with BufferOut

  "sync" should "respect repository root" in repoChee() { setup =>
    intercept[UserError] {
      sync.run(setup, TestInfo.images(0).parent.pathAsString)
    }
  }

  it should "add new files" in bothChee() { setup =>
    val target = (setup.files / "repo").createDirectories()
    copyFile(TestInfo.images(2), Some(target)) { _ =>
      val (_, Nil) = sync.run(setup, target.pathAsString)
      val (out, Nil) = findLisp(setup)
      out should have size (1)
      out(0) should include (s""":checksum "${TestInfo.images(2).sha256.toLowerCase}"""")
      out(0) should include (s""":path "${(target / TestInfo.images(2).name).path}"""")
    }
  }

  it should "update changed files" in bothChee() { setup =>
    val target = (setup.files / "repo").createDirectories()
    copyFile(TestInfo.images(2), Some(target)) { _ =>
      addCmd.run(setup, target.pathAsString)
      val (before, Nil) = find.run(setup, "-p", "~#checksum~%")
      before should have size (1)
      before(0) should be (TestInfo.images(2).sha256.toLowerCase)

      (target / TestInfo.images(2).name).delete()
      TestInfo.images(1).copyTo(target / TestInfo.images(2).name)
      val (_, Nil) = sync.run(setup, target.pathAsString)

      val (after, Nil) = find.run(setup, "-p", "~#checksum~%")
      after should have size (1)
      after(0) should include (TestInfo.images(1).sha256.toLowerCase)
    }
  }

  it should "delete non existing files" in bothChee(addImages) { setup =>
    val (before, Nil) = findLisp(setup)
    setup.files.list.take(2).foreach(_.delete())
    val (_, Nil) = sync.run(setup, setup.files.pathAsString)
    val (after, Nil) = findLisp(setup)
    after.size should be (before.size -2)
  }

  it should "delete non existing files only for given paths" in bothChee(addImages) { setup =>
    val files = setup.files.list.toList
    val file = files.head
    files.tail.foreach(_.delete())
    val (_, Nil) = sync.run(setup, file.pathAsString)
    val (out, Nil) = findLisp(setup)
    out should have size (files.size.toLong)
  }

  it should "update changed encrypted files" in bothChee() { setup => //broken
    val target = (setup.files / "repo").createDirectories()
    val source = target / TestInfo.images(2).name
    copyFile(TestInfo.images(2), Some(target)) { _ =>
      addCmd.run(setup, target.pathAsString)
      val (before, Nil) = find.run(setup, "-p", "~#checksum~%")
      before should have size (1)
      before(0) should be (TestInfo.images(2).sha256.toLowerCase)
      val (added, Nil) = find.run(setup, "-p", "~#added")
      added(0) should fullyMatch regex ("[0-9]+")

      source.delete()
      encryptFile(TestInfo.images(1), "test".toCharArray, Some(source)) { enc =>
        withNewFile { passfile =>
          passfile `<<` "test"
          val (_, Nil) = sync.run(setup, "-d", "--method", "password", "--passphrase", passfile.pathAsString, target.pathAsString)

          val (after, Nil) = findLisp(setup)
          after should have size (1)
          after(0) should include (TestInfo.images(1).sha256.toLowerCase)
          after(0) should include (s""":path "${enc.pathAsString}"""")
          after(0) should include (s""":filename "${source.name}"""")
          after(0) should include (s""":added ${added(0)}""")
        }
      }
    }
  }

  it should "rename unchanged encrypted files" in bothChee() { setup =>
    val target = (setup.files / "repo").createDirectories()
    val source = target / TestInfo.images(2).name
    copyFile(TestInfo.images(2), Some(target)) { _ =>
      addCmd.run(setup, target.pathAsString)
      val (before, Nil) = find.run(setup, "-p", "~#checksum~%")
      before should have size (1)
      before(0) should be (TestInfo.images(2).sha256.toLowerCase)
      val (added, Nil) = find.run(setup, "-p", "~#added")
      added(0) should fullyMatch regex ("[0-9]+")

      encryptFile(source, "test".toCharArray) { enc =>
        source.delete()
        withNewFile { passfile =>
          passfile `<<` "test"
          val (_, Nil) = sync.run(setup, "-d", "--method", "password", "--passphrase", passfile.pathAsString, target.pathAsString)
          val (after, Nil) = findLisp(setup)
          after should have size (1)
          after(0) should include (TestInfo.images(2).sha256.toLowerCase)
          after(0) should include (s""":path "${enc.pathAsString}"""")
          after(0) should include (s""":filename "${source.name}"""")
          after(0) should include (s""":added ${added(0)}""")
        }
      }
    }
  }

  it should "rename unchanged decrypted files" in bothChee() { setup =>
    val target = (setup.files / "repo").createDirectories()
    combine(withNewFile, encryptFile(TestInfo.images(2), "test".toCharArray, Some(target))) { (passfile, enc) =>
      // add encrypted file
      passfile `<<` "test"
      addCmd.run(setup, "-d", "--method", "password", "--passphrase", passfile.pathAsString, target.pathAsString)
      val (before, Nil) = find.run(setup, "-p", "~#checksum ~#encrypted~%")
      before should have size (1)
      before(0) should startWith (TestInfo.images(2).sha256.toLowerCase)
      before(0) should endWith (CheeCrypt.passwordEncryptExtension)

      // decrypt outside of chee, remove encrypted file
      decryptFile(enc, "test".toCharArray) { dec =>
        enc.delete()
        enc.exists should be (false)
        dec.name should be (TestInfo.images(2).name)

        // sync the new file
        val (out, Nil) = sync.run(setup, target.pathAsString)

        // verfiy
        val (after, Nil) = findLisp(setup)
        after should have size (1)
        after(0) should include (s""":checksum "${TestInfo.images(2).sha256.toLowerCase}"""")
        after(0) should include (s""":path "${dec.pathAsString}"""")
      }
    }
  }

  it should "refuse for non existing dirs" in bothChee() { setup =>
    val dir = setup.files / "non-existing"

    intercept[UserError] {
      sync.run(setup, "-r", dir.pathAsString)
    }
    intercept[UserError] {
      sync.run(setup, "--reindex", dir.pathAsString)
    }

    addLocation(dir, setup)
    findLisp(setup)._1.size should be > (0)

    dir.delete()
    intercept[UserError] {
      sync.run(setup, "--reindex", "--everything")
    }
  }

  it should "update location after sync" in bothChee() { setup =>
    for (file <- TestInfo.images.take(2)) {
      val target = setup.files / "dir" / file.name
      target.parent.createDirectories()
      file.copyTo(target)
    }
    addCmd.run(setup, "-r", (setup.files / "dir").pathAsString)

    for (file <- TestInfo.images.drop(2)) {
      file.copyTo(setup.files / file.name)
    }
    val (_, Nil) = sync.run(setup, "-r", setup.files.pathAsString)
    val (out, Nil) = find.run(setup, "-p", "~#location~%")
    out.toSet should have size (1)
    out(0) should be (setup.files.pathAsString)
  }
}
