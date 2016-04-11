package chee.cli

import org.scalatest._

class WrapLinesTest extends FlatSpec with Matchers {

  val text = "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed" +
  " diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam" +
  " erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et" +
  " ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem" +
  " ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing" +
  " elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna" +
  " aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores" +
  " et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem" +
  " ipsum dolor sit amet."

  "wrapLines" should "not exceed maximum length" in {
    val lines = wrapLines(72)(text)
    for (l <- lines.split("\n")) {
      l.length should be <= (72)
    }
  }

  it should "exceed maximum length if it must" in {
    val expected = text.replace(' ', '\n')
    wrapLines(1)(text) should be (expected)
    wrapLines(-1)(text) should be (expected)
    wrapLines(0)(text) should be (expected)

    val noWhiteSpace = "uiaegfiaeuidatrenuitranedutiraenvlc"
    wrapLines(5)(noWhiteSpace) should be (noWhiteSpace)

    val toLongWord = "uiaeuiduiatreniaeia uiaei etra niaa uiiii"
    wrapLines(10)(toLongWord) should be ("uiaeuiduiatreniaeia\nuiaei etra\nniaa uiiii")
  }

  it should "do nothing on already good text" in {
    val lines1 = wrapLines(72)(text)
    val lines2 = wrapLines(72)(lines1)
    lines1 should be (lines2)
  }

  it should "preserve newlines" in {
    val text = "uiae\niaue\nuiae\nuiae uiae\nuiae\nuiae\nuiae\nuiae"
    wrapLines(10)(text) should be (text)
    wrapLines(15)(text) should be (text)
    wrapLines(20)(text) should be (text)
  }
}
