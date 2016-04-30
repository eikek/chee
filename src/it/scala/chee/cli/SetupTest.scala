package chee.cli

import org.scalatest._
import chee.it._
import chee.conf._

class SetupTest extends FlatSpec with Matchers with CommandSetup {

  "globalChee" should "make config with isolated chee dirs" in globalChee { setup =>
    setup.cfg.getFile("chee.configdir") should be (setup.userDir / ".chee-dir")
    setup.cfg.getFile("chee.system-config") should be (setup.userDir / ".chee-dir" / "system.cfg")
    setup.cfg.getFile("chee.workingdir") should be (setup.userDir / ".chee-dir" / "work")
    setup.cfg.getFile("chee.tmpdir") should be (setup.userDir / ".chee-dir" / "work" / "tmp")
  }

  "globalCheeWithImages" should "add a location with images" in globalCheeWithImages { setup =>
    val linfo = new LocationInfo with BufferOut
    val (stdout, Nil) = linfo.run(setup)
    stdout(0) should startWith (setup.files.pathAsString)
  }

  "repoRoot" should "add repo.root property" in repoRoot { setup =>
    setup.cfg.getRepoRoot should be (Some(setup.userDir))
    (setup.userDir / ".chee").exists should be (true)
  }
}
