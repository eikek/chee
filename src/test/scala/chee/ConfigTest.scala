package chee

import org.scalatest._
import chee.conf._

class ConfigTest extends FlatSpec with Matchers {

  val cfg = CheeConf.defaultConfig

  "workingdir" should "be target folder" in {
    cfg.getFile("chee.workingdir") should be (TestInfo.targetDir / "test")
    cfg.getFile("chee.configdir") should be (TestInfo.targetDir / "test")
  }

  "tmpdir" should "not be changed in tests" in {
    cfg.getFile("chee.tmpdir") should be (TestInfo.targetDir / "test" / "tmp")
    System.getProperty("java.io.tmpdir") should not be (cfg.getString("chee.tmpdir"))
  }

  "getCommand" should "return Nil if empty string" in {
    cfg.getCommand("chee.crypt.default-passphrase-command") should be (Nil)
  }
}
