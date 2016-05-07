package chee.cli

import chee.UserError
import chee.it.{ BufferOut, CommandSetup, FindHelper }
import org.scalatest._

class MetaAttachTest extends FlatSpec with Matchers with CommandSetup with FindHelper {

  def attach = new MetaAttach with BufferOut
  def metaFind = new MetaFind with BufferOut

  "attach" should "add and change metadata" in bothChee(addImages) { setup =>
    val (out, Nil) = findLisp(setup)
    val (_, Nil) = attach.run(setup, "--tags", "eagle,sheep")

    val (out2, Nil) = findLisp(setup, "tag:sheep")
    out.map(_.replace(":tag nil", ":tag \"|eagle|sheep|\"")) should be (out2)

    val (_, Nil) = attach.run(setup, "--comment", "scoobeedoobeedoo")
    val (out3, Nil) = findLisp(setup, "tag:eagle")
    out2.map(_.replace(":comment nil", ":comment \"scoobeedoobeedoo\"")) should be (out3)

    val (out4, Nil) = findLisp(setup, "comment:*bee*")
    out4 should be (out3)

    val (_, Nil) = attach.run(setup, "--comment", "yeehaaa", "--tags", "cat,dog")
    List(findLisp(setup),
      findLisp(setup, "comment:*ee*"),
      findLisp(setup, "tag:dog")).reduce { (m1, m2) =>
      m1 should be (m2)
      m2
    }
  }

  it should "drop tags" in bothChee(addImages) { setup =>
    val (_, Nil) = attach.run(setup, "--comment", "scoobeedoobeedoo")
    val (out, Nil) = findLisp(setup)
    val (_, Nil) = attach.run(setup, "--tags", "eagle,sheep")

    val (out2, Nil) = findLisp(setup, "tag:sheep")
    out2.size should be (out.size)

    val (_, Nil) = attach.run(setup, "--drop-tags")
    val (out3, Nil) = findLisp(setup, "comment:*bee*")
    out3 should be (out)
  }

  it should "drop comments" in bothChee(addImages) { setup =>
    val (_, Nil) = attach.run(setup, "--tags", "eagle,sheep")
    val (out, Nil) = findLisp(setup)
    val (_, Nil) = attach.run(setup, "--comment", "scoobeedoobeedoo")

    val (out2, Nil) = findLisp(setup, "comment:*bee*")
    out2.size should be (out.size)

    val (_, Nil) = attach.run(setup, "--drop-comment")
    val (out3, Nil) = findLisp(setup, "tag:eagle")
    out3 should be (out)
  }

  it should "drop entries" in bothChee(addImages) { setup =>
    val (_, Nil) = attach.run(setup, "--comment", "scoobeee", "--tags", "sheep,eagle")
    val (xout, Nil) = findLisp(setup)
    val (out, Nil) = metaFind.run(setup, "-p", "lisp", "checksum?")
    out.size should be (xout.size)

    val (_, Nil) = attach.run(setup, "--drop-all", "--skip", "2")
    val (out2, Nil) = metaFind.run(setup, "-p", "lisp", "checksum?")
    out2.size should be (out.size - 2)
  }

  it should "refuse if drop and change tags/comments" in bothChee() { setup =>
    intercept[UserError] {
      attach.run(setup, "--drop-tags", "--tags", "eagle")
    }
    intercept[UserError] {
      attach.run(setup, "--drop-comment", "--comment", "scoobeedoo")
    }
  }

  it should "refuse if tag are invalid" in bothChee() { setup =>
    intercept[UserError] {
      attach.run(setup, "--tags", "a ha") //whitespace
    }
    intercept[UserError] {
      attach.run(setup, "--tags", "a\n ha")
    }
    intercept[UserError] {
      attach.run(setup, "--tags", "a;ha") // semi colon
    }
    intercept[UserError] {
      attach.run(setup, "--tags", "a|ha") // pipe
    }
  }
}
