package chee.it

import chee.cli.Info
import org.scalatest._
import chee.conf._

class SetupTest extends FlatSpec with Matchers with CommandSetup with FindHelper {

  "globalChee" should "make config with isolated chee dirs" in globalChee() { setup =>
    setup.cfg.getFile("chee.configdir") should be (setup.userDir / ".chee-dir")
    setup.cfg.getFile("chee.system-config") should be (setup.userDir / ".chee-dir" / "system.cfg")
    setup.cfg.getFile("chee.workingdir") should be (setup.userDir / ".chee-dir" / "work")
    setup.cfg.getFile("chee.tmpdir") should be (setup.userDir / ".chee-dir" / "work" / "tmp")
  }

  "withImages" should "add a location with images" in bothChee(addImages) { setup =>
    val linfo = new Info with BufferOut
    val (stdout, Nil) = linfo.run(setup)
    stdout(0) should startWith (setup.files.pathAsString)

    setup.files.list.toList should have size (4)
    val (out, Nil) = findLisp(setup)
    out should have size (4)
  }

  "repoRoot" should "add repo.root property" in repoChee() { setup =>
    setup.cfg.getRepoRoot should be (Some(setup.userDir))
    (setup.userDir / ".chee").exists should be (true)
  }

  "LineBuffer" should "append lines" in {
    LineBuffer().putln("a").putln("b").putln("c").lines should be (Vector("a", "b", "c"))
    LineBuffer().put("a").putln("b").put("c").lines should be (Vector("ab", "c"))
    LineBuffer().put("a\nb\n").put("c").putln("de").lines should be (
      Vector("a", "b", "cde"))
    LineBuffer().put("a\nb\n\n").lines should be (Vector("a", "b", ""))
    LineBuffer().put("\n\n\n\n").lines should be (Vector("", ""))
    LineBuffer().put("\n").put("\n").put("\n").lines should be (Vector(""))
  }
}
