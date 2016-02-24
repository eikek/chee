package chee

import org.scalatest._
import chee.properties._
import chee.properties.Ident._
import better.files._
import chee.CheeConf.Implicits._

class ConfigTest extends FlatSpec with Matchers {

  val cfg = CheeConf.config

  "workingdir" should "be target folder" in {
    cfg.getFile("chee.workingdir") should be (TestInfo.targetDir)
    cfg.getFile("chee.configdir") should be (TestInfo.targetDir)
  }

  "tmpdir" should "not be changed in tests" in {
    cfg.getFile("chee.tmpdir") should be (TestInfo.targetDir / "tmp")
    System.getProperty("java.io.tmpdir") should not be (cfg.getString("chee.tmpdir"))
  }
}
