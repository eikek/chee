package chee

import scala.io.Source
import org.scalatest._
import better.files._

class DocTest extends FlatSpec with Matchers {

  val testFolder = file"target"/"testfiles"

  "firstParagraph" should "return text between first two empty lines" in {
    val text = """== headline
        |
        | this is
        | the text.
        |
        | and this is not.""".stripMargin

    CheeDoc.firstParagraph(text.split("\n").toIterator) should be ("this is the text.")
  }

  "commandSummaryLine" should "find subcommands" in {
    CheeDoc.commandSummaryLine("collection", Some("edit")) should be (
      Some("Edits a collection or creates a new one."))

    CheeDoc.commandSummaryLine("import") should be (
      Some("Imports files from a folder into a location."))
  }

  it should "return None for non existing commands" in {
    CheeDoc.commandSummaryLine("abc") should be (None)
    CheeDoc.commandSummaryLine("collection", Some("abc")) should be (None)
  }

  "doc util" should "find existing pages" in {
    val Some(s) = CheeDoc.findPage("cmd-help", "adoc")
    val Some(_) = CheeDoc.findCommandPage("help", "html")
    val Some(_) = CheeDoc.findAboutPage("collection", "adoc")
  }

  "doc util" should "find None for non-existing pages" in {
    CheeDoc.findPage("abc", "html") should be (None)
    CheeDoc.findPage("cmd-help", "pdf") should be (None)
  }

  "copy page" should "create a file with content" in {
    if (testFolder.exists) testFolder.delete()
    val Some(f) = CheeDoc.copyPage("cmd-help", "html", testFolder/"target.html")
    f.isRegularFile should be (true)
    f should be (testFolder/"target.html")
    f.lines.mkString should be (getPageContent("cmd-help","html"))
  }

  "copy page" should "write into a directory" in {
    if (testFolder.exists) testFolder.delete()
    val Some(f) = CheeDoc.copyPage("cmd-help", "html", testFolder/"a"/"b")
    f.isRegularFile should be (true)
    f should be (testFolder/"a"/"b"/"cmd-help.html")
    f.lines.mkString should be (getPageContent("cmd-help","html"))
  }

  "copy page" should "overwrite existing files" in {
    if (testFolder.exists) testFolder.delete()
    val target = testFolder/"myfile.html"
    target.createIfNotExists(createParents = true)
    target.appendLine("some other stuff")
    target.lines.mkString should be ("some other stuff")
    val Some(f) = CheeDoc.copyPage("cmd-help", "html", target)
    f.lines.mkString should be (getPageContent("cmd-help","html"))
  }

  "copy page" should "use the existing directory no matter the name" in {
    if (testFolder.exists) testFolder.delete()
    val target = testFolder/"myfile.html"
    target.createIfNotExists(asDirectory = true)
    val Some(f) = CheeDoc.copyPage("cmd-help", "html", target)
    f should be (target/"cmd-help.html")
  }

  def getPageContent(name: String, format: String): String =
    Source.fromURL(CheeDoc.findPage(name, format).get).getLines.mkString
}
