package chee

import java.time.Instant
import better.files.File
import org.scalatest._
import org.scalatest.matchers._

trait MoreMatcher {

  implicit val _fileExistence = new enablers.Existence[File] {
    def exists(f: File) = f.exists
  }

  class BeforeMatcher(right: Instant) extends Matcher[Instant] {
    def apply(left: Instant) = MatchResult(
      left.isBefore(right),
      s"'$left' is not before '$right'",
      s"'$left' is before '$right'"
    )
  }

  class AfterMatcher(right: Instant) extends Matcher[Instant] {
    def apply(left: Instant) = MatchResult(
      left.isAfter(right),
      s"'$left' is not after '$right'",
      s"'$left' is after '$right'"
    )
  }

  def beAfter(i: Instant) = new AfterMatcher(i)
  def beBefore(i: Instant) = new BeforeMatcher(i)
}

object MoreMatcher extends MoreMatcher
