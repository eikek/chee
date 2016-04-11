package chee.crypto

import chee.TestInfo
import chee.properties._
import org.scalatest._

class CheeCryptTest extends FlatSpec with Matchers with chee.FileLoan {

  val password = "testchee123"

  val pubKeyAscii = TestInfo.gnupgDir / "pubkeys.txt"
  val pubKeyGpg = TestInfo.gnupgDir / "pubkeys.gpg"
  val secKeysAscii = TestInfo.gnupgDir / "secret-keys.txt"
  val secKeysGpg = TestInfo.gnupgDir / "secret-keys.gpg"

  "isPasswordEncrypted" should "return true for pbc files" in {
    val m = LazyMap(Ident.path -> "/mnt/test/img_0456.JPG.pbc")
    CheeCrypt.isPasswordEncrypted.result(m) should be (true)
  }

  it should "return false for pkc files" in {
    val m = LazyMap(Ident.path -> "/mnt/test/img_0456.JPG.pkc")
    CheeCrypt.isPasswordEncrypted.result(m) should be (false)
  }

  it should "return false for non-encrypted files" in {
    val m = LazyMap(Ident.path -> "/mnt/test/img_0456.JPG")
    CheeCrypt.isPasswordEncrypted.result(m) should be (false)
  }

  "isPublicKeyEncrypted" should "return true for pkc files" in {
    val m = LazyMap(Ident.path -> "/mnt/test/img_0456.JPG.pkc")
    CheeCrypt.isPublicKeyEncrypted.result(m) should be (true)
  }

  it should "return false for pbc files" in {
    val m = LazyMap(Ident.path -> "/mnt/test/img_0456.JPG.pbc")
    CheeCrypt.isPublicKeyEncrypted.result(m) should be (false)
  }

  it should "return false for non-encrypted files" in {
    val m = LazyMap(Ident.path -> "/mnt/test/img_0456.JPG")
    CheeCrypt.isPublicKeyEncrypted.result(m) should be (false)
  }

}
