package chee.crypto

import chee.TestInfo
import org.scalatest._

class KeyFindTest extends FlatSpec with Matchers with chee.FileLoan {

  val password = "testchee123"

  val pubKeyAscii = TestInfo.gnupgDir / "pubkeys.txt"
  val pubKeyGpg = TestInfo.gnupgDir / "pubkeys.gpg"
  val secKeysAscii = TestInfo.gnupgDir / "secret-keys.txt"
  val secKeysGpg = TestInfo.gnupgDir / "secret-keys.gpg"

  "findPublicKey" should "find existing keys by user id" in {
    KeyFind.findPublicKey(pubKeyAscii, "test@chee")
    KeyFind.findPublicKey(pubKeyAscii, "test2@chee")
    KeyFind.findPublicKey(pubKeyGpg, "test@chee")
    KeyFind.findPublicKey(pubKeyGpg, "test2@chee")
  }

  it should "find existing keys by id" in {
    KeyFind.findPublicKey(pubKeyAscii, "eac52e05")
    KeyFind.findPublicKey(pubKeyAscii, "2DDD346E")
    KeyFind.findPublicKey(pubKeyGpg, "eac52e05")
    KeyFind.findPublicKey(pubKeyGpg, "2DDD346E")
  }

  "findSecretKey" should "find keys by user id" in {
    KeyFind.findSecretKey(secKeysAscii, "test@chee")
    KeyFind.findSecretKey(secKeysAscii, "test2@chee")
    KeyFind.findSecretKey(secKeysGpg, "test@chee")
    KeyFind.findSecretKey(secKeysGpg, "test2@chee")
  }

  it should "find existing keys by id" in {
    KeyFind.findSecretKey(secKeysAscii, "eac52e05")
    KeyFind.findSecretKey(secKeysAscii, "2DDD346E")
    KeyFind.findSecretKey(secKeysGpg, "eac52e05")
    KeyFind.findSecretKey(secKeysGpg, "2DDD346E")
  }

  "extractPrivateKey" should "find private key with pass" in {
    KeyFind.extractPrivateKey(KeyFind.findSecretKey(secKeysAscii, "eac52e05"), password.toCharArray())
    KeyFind.extractPrivateKey(KeyFind.findSecretKey(secKeysAscii, "2ddd346e"), password.toCharArray())
  }
}
