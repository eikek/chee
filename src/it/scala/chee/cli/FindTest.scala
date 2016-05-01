package chee.cli

import chee.it.CommandSetup.Setup
import java.util.UUID
import org.scalatest._
import chee.it._

class FindTest extends FlatSpec with Matchers with CommandSetup with FindHelper {

  "find" should "return --first 2 files" in bothChee(addImages) { setup =>
    val (stdout, Nil) = find.run(setup, "--first", "2")
    stdout should have length (2)
  }

  it should "--skip 2 files" in bothChee(addImages) { setup =>
    val (all, Nil) = find.run(setup)
    val (stdout, Nil) = find.run(setup, "--skip", "2")
    stdout.length should be (all.length - 2)
  }

  it should "find non-indexed files" in bothChee(addImages) { setup =>
    val (stdout, Nil) = find.run(setup, "-f", setup.userDir.pathAsString, "-r", "--indexed", "false")
    stdout should be (Nil)
  }

  it should "find indexed files" in bothChee(addImages) { setup =>
    val (stdout, Nil) = find.run(setup, "-f", setup.userDir.pathAsString, "-r", "--indexed", "true")
    val (all, Nil) = find.run(setup, "-f", setup.userDir.pathAsString, "-r")
    stdout should be (all)
  }

  it should "find same files after moving repository" in repoChee(addImages) { setup =>
    val (out1, Nil) = findLisp(setup)
    setup.dirs.repoRoot match {
      case Some(repo) =>
        val newName = repo.name + "_2"
        repo.moveTo(repo sibling newName)
        val newSetup = Setup(repoSetup(repo sibling newName))
        val (out2, Nil) = findLisp(newSetup)
        out2 should be (out1.map(s => s.replace(repo.name, newName)))

      case _ => sys.error("no repository root")
    }
  }
}
