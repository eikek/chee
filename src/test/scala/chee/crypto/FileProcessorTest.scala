package chee.crypto

import chee.TestInfo
import org.bouncycastle.openpgp.PGPDataValidationException
import org.scalatest._

class FileProcessorTest extends FlatSpec with Matchers with chee.FileLoan {

  val password = "testchee123"

  val pubkeysPlain = TestInfo.gnupgDir / "pubkeys.txt"
  val seckeysPlain = TestInfo.gnupgDir / "secret-keys.txt"

  "de/encryptSymmetric" should "be decryptable" in withNewFile { f =>
    f.write("hello world")
    withNewFile { out =>
      out.exists should be (false)
      FileProcessor.encryptSymmetric(f, out, "test".toCharArray(), Algorithm.Blowfish)
      out.exists should be (true)
      f.delete()
      f.exists should be (false)
      FileProcessor.decryptSymmetric(out, f, "test".toCharArray())
      f.exists should be (true)
      f.contentAsString should be ("hello world")
    }
  }

  it should "throw when decrypting with wrong password" in withNewFile { f =>
    f.write("hello world")
    withNewFile { out =>
      out.exists should be (false)
      FileProcessor.encryptSymmetric(f, out, "test".toCharArray(), Algorithm.Blowfish)
      out.exists should be (true)
      f.delete()
      f.exists should be (false)

      intercept[PGPDataValidationException] {
        FileProcessor.decryptSymmetric(out, f, "sest".toCharArray())
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
      FileProcessor.decryptPubkey(out, seckeysPlain, password.toCharArray(), f)
      f.exists should be (true)
      f.contentAsString should be ("hello world")
    }
  }

}
