package chee.crypto

import chee.TestInfo
import chee.properties.ChecksumExtract
import org.bouncycastle.openpgp.PGPDataValidationException
import org.scalatest._

class FileProcessorTest extends FlatSpec with Matchers with chee.FileLoan {

  // password to private keys
  val password = "testchee123".toCharArray

  val pubkeysPlain = TestInfo.gnupgDir / "pubkeys.txt"
  val seckeysPlain = TestInfo.gnupgDir / "secret-keys.txt"

  "de/encryptSymmetric" should "be decryptable" in withNewFile { f =>
    f.write("hello world")
    withNewFile { out =>
      out.exists should be (false)
      FileProcessor.encryptSymmetric(f, out, password, Algorithm.Blowfish)
      out.exists should be (true)
      f.delete()
      f.exists should be (false)
      FileProcessor.decryptSymmetric(out, f, password)
      f.exists should be (true)
      f.contentAsString should be ("hello world")
    }
  }

  it should "throw when decrypting with wrong password" in withNewFile { f =>
    f.write("hello world")
    withNewFile { out =>
      out.exists should be (false)
      FileProcessor.encryptSymmetric(f, out, password, Algorithm.Blowfish)
      out.exists should be (true)
      f.delete()
      f.exists should be (false)

      intercept[PGPDataValidationException] {
        FileProcessor.decryptSymmetric(out, f, "sest".toCharArray())
      }
    }
  }

  it should "decrypt to same checksum" in {
    Algorithm.all.foreach { case (name, algo) =>
      info(s"Algorithm: $name")
      withNewFile { f =>
        f write "hello world"
        val checksum = ChecksumExtract.checksum(f)
        withNewFile { out =>
          FileProcessor.encryptSymmetric(f, out, password, algo)
          f.delete()
          FileProcessor.decryptSymmetric(out, f, password)
          ChecksumExtract.checksum(f) should be (checksum)
        }
      }
    }
  }

  "de/encryptPubkey" should "encrypt and decrypt" in withNewFile { f =>
    f.write("hello world")
    withNewFile { out =>
      out.exists should be (false)
      FileProcessor.encryptPubkey(f, KeyFind.findPublicKey(pubkeysPlain, "test@chee"), out)
      out.exists should be (true)
      f.delete()
      f.exists should be (false)
      FileProcessor.decryptPubkey(out, seckeysPlain, password, f)
      f.exists should be (true)
      f.contentAsString should be ("hello world")
    }
  }

  it should "decrypt to same checksum" in {
    withNewFile { f =>
      f write "hello world"
      val checksum = ChecksumExtract.checksum(f)
      withNewFile { out =>
        FileProcessor.encryptPubkey(f, KeyFind.findPublicKey(pubkeysPlain, "test@chee"), out)
        f.delete()
        FileProcessor.decryptPubkey(out, seckeysPlain, password, f)
        ChecksumExtract.checksum(f) should be (checksum)
      }
    }
  }

}
