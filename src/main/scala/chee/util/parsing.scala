package chee.util

import fastparse.WhitespaceApi
import fastparse.all._

object parsing {

  lazy val digit: P0 = CharIn('0' to '9')

  lazy val identString: P0 =
    CharIn('a' to 'z', 'A' to 'Z', '0' to '9', "_-").rep(1)

  lazy val WS = CharIn("\n\t ")

  def alphaPlus(more: Seq[Char]*): P0 = {
    val alpha: Seq[Seq[Char]] = Seq(('a' to 'z'), ('A' to 'Z'))
    CharIn(alpha ++ more: _*)
  }

  def alphanumPlus(more: Seq[Char]): P0 =
    alphaPlus('0' to '9', more)

  def ic(s: String) = IgnoreCase(s)

  def CharNotIn(chars: Seq[Char]*) = chars match {
    case Seq(Seq(c)) => !c.toString ~ AnyChar
    case _ => !CharIn(chars: _*) ~ AnyChar
  }

  def quotedString(quote: Char, escapeChar: Char = '\\'): P[String] = {
    val p = P(escapeChar.toString ~ AnyChar.!)
    val q = P(!CharIn(Seq(escapeChar, quote)) ~ AnyChar.!)

    val str: P[String] = P((p | q).rep).map(_.mkString)

    P(quote.toString ~ str ~ quote.toString)
  }

  final implicit class CustomApi[T](self: P[T]) {
    def times(n: Int) = self.rep(min = n, max = n)

    def parseAll(in: String): Either[String, T] =
      P(self ~ End).parse(in) match {
        case fastparse.core.Parsed.Success(c, _) => Right(c)
        case f => Left(f.toString)
      }

    def parsePrefix(in: String): Either[String, T] =
      self.parse(in) match {
        case fastparse.core.Parsed.Success(c, _) => Right(c)
        case f => Left(f.toString)
      }
  }

  val IgnoreWhitespace = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(Seq(' ', '\t')).rep)
  }
}
