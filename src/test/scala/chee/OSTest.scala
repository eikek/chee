package chee

import org.scalatest._

class OSTest extends FlatSpec with Matchers {

  "Run.tokenize" should "tokenize commands" in {
    OS.Command("firefox %s", Seq("bla.html")).get should be (
      Seq("firefox", "bla.html")
    )

    OS.Command("firefox -t 32 %s", Seq("bla.html")).get should be (
      Seq("firefox", "-t", "32", "bla.html")
    )

    OS.Command("firefox %s %s", Seq("bla.html")).get should be (
      Seq("firefox", "%s", "bla.html")
    )

    OS.Command("firefox -name 'here there where' %s", Seq("bla.html")).get should be (
      Seq("firefox", "-name", "here there where", "bla.html")
    )

    OS.Command("firefox ", Seq("bla.html")).get should be (
      Seq("firefox")
    )

    OS.Command("", Seq("bla.html")).get should be ('empty)
  }
}
