package chee.cli

import org.scalatest._
import chee.it._

class FindTest extends FlatSpec with Matchers with CommandSetup {

  def find = new Find with BufferOut

  "find" should "return --first 2 files" in globalCheeWithImages { setup =>
    val (stdout, Nil) = find.run(setup, "--first", "2")
    stdout should have length (3)
  }

  it should "--skip 2 files" in globalCheeWithImages { setup =>
    val (all, Nil) = find.run(setup)
    val (stdout, Nil) = find.run(setup, "--skip", "2")
    stdout.length should be (all.length - 2)
  }

  it should "find non-indexed files" in globalCheeWithImages { setup =>
    val (stdout, Nil) = find.run(setup, "-f", setup.userDir.pathAsString, "-r", "--indexed", "false")
    stdout should be (Nil)
  }

  it should "find indexed files" in globalCheeWithImages { setup =>
    val (stdout, Nil) = find.run(setup, "-f", setup.userDir.pathAsString, "-r", "--indexed", "true")
    val (all, Nil) = find.run(setup, "-f", setup.userDir.pathAsString, "-r")
    stdout should be (all)
  }

}
