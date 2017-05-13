package chee.cli

import better.files.File
import chee.crypto.CheeCrypt
import org.scalatest._
import chee.{FileLoan, TestInfo}
import chee.it._
import chee.util.files._

class CryptTest extends FlatSpec with Matchers with CommandSetup with FindHelper with FileLoan {

  val pubkeysPlain = TestInfo.gnupgDir / "pubkeys.txt"
  val seckeysPlain = TestInfo.gnupgDir / "secret-keys.txt"
  val keyId = "test@chee"
  val pass = "testchee123".toCharArray

  def encrypt = new Encrypt with BufferOut
  def decrypt = new Decrypt with BufferOut

  def withPassFile(code: File => Any) =
    withNewFile { f =>
      f.write(String.valueOf(pass))
      code(f)
    }

  "encrypt" should "encrypt using a password" in bothChee(addImages) { setup =>
    withPassFile { passfile =>
      val (_, Nil) = encrypt.run(setup, "--method", "password", "--passphrase", passfile.pathAsString)
      setup.files.list.forall(_.name endsWith CheeCrypt.passwordEncryptExtension) should be (true)

      val (filenames, Nil) = find.run(setup, "-p", "~#filename~%")
      setup.files.list.map(_.stripExtension.name).toList.sorted should be (filenames.sorted)
    }
  }

  it should "encrypt using a public key" in bothChee(addImages) { setup =>
    val (_, Nil) = encrypt.run(setup, "--method", "pubkey", "--key-file", pubkeysPlain.pathAsString, "--key-id", keyId)
    setup.files.list.forall(_.name endsWith CheeCrypt.publicKeyEncryptExtension) should be (true)

    val (filenames, Nil) = find.run(setup, "-p", "~#filename~%")
    setup.files.list.map(_.stripExtension.name).toList.sorted should be (filenames.sorted)
  }

  "decrypt" should "decrypt using a password" in bothChee(addImages) { setup =>
    withPassFile { passfile =>
      val (before, Nil) = findLisp(setup)
      val (_, Nil) = encrypt.run(setup, "--method", "password", "--passphrase", passfile.pathAsString)
      val (x, Nil) = findLisp(setup)
      x should not be (before)

      val (_, Nil) = decrypt.run(setup, "--method", "password", "--passphrase", passfile.pathAsString)
      val (after, Nil) = findLisp(setup)
      before should be (after)
    }
  }

  it should "decrypt using a private key" in bothChee(addImages) { setup =>
    val (before, Nil) = findLisp(setup)
    val (_, Nil) = encrypt.run(setup, "--method", "pubkey", "--key-file", pubkeysPlain.pathAsString, "--key-id", keyId)
    val (x, Nil) = findLisp(setup)
    x should not be (before)

    withPassFile { pass =>
      val (_, Nil) = decrypt.run(setup, "--method", "pubkey", "--key-file", seckeysPlain.pathAsString, "--secret-key-pass", pass.pathAsString)
      val (after, Nil) = findLisp(setup)
      before should be (after)
    }
  }
}
