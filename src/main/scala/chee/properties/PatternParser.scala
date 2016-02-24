package chee.properties

import scala.util.parsing.combinator.RegexParsers
import Patterns._
import chee.Processing

/** The Parser[Ident => Pattern] is only needed for the loop body, but
  * declared for all parsers to reuse them in the loop body.
  *
  * The placeholder (value) must be replaced by the identifier of the
  * current iteration. This is done by providing it to the
  * function. For the special identifier `value` (only valid inside
  * the loop), the identifier provided to the function is used,
  * otherwise the parsed identifier is used.
  */
final class PatternParser(idents: Iterable[Ident]) extends RegexParsers {
  override val whiteSpace = "".r

  def lift(p: Parser[Pattern]): Parser[Ident => Pattern] = p.map(id => _)

  val tilde: Parser[String] = "~" ~> "~"

  val rawValue: Parser[Pattern] = rep1(tilde | """[^~]""".r) ^^ {
    list => seqq(list.map(raw))
  }

  val lookupValue: Parser[Ident => Pattern] =
    "~#" ~> Ident.identRegex.r ~ opt("~f" ~> "[^~]+".r) ^^ {
      case "value" ~ format => id => lookup(id, format)
      case s ~ format => _ => lookup(Ident(s), format)
    }

  val readableValue: Parser[Ident => Pattern] =
    "~:" ~> Ident.identRegex.r ^^ {
      case "value" => id => readable(id)
      case s => _ => readable(Ident(s))
    }

  val newlineValue: Parser[Pattern] = "~%" ^^ { _ => newline }

  val emptyValue: Parser[Pattern] = "~." ^^ { _ => empty }

  val simpleDirective: Parser[Ident => Pattern] =
    lift(emptyValue) | lift(newlineValue) | lookupValue | readableValue | lift(rawValue)

  def quotedValue: Parser[Ident => Pattern] = "~" ~> ("\"" | "'") ~ directive ^^ {
    case q ~ directive => id => quote(q.charAt(0), directive(id))
  }

  def sequence: Parser[Ident => Pattern] = "~{" ~> rep1(directive) <~ "~}" ^^ {
    list => id => seqq(list.map(_(id)))
  }

  def existsPred: Parser[Ident => MapGet[Either[String, Boolean]]] = Ident.identRegex.r ^^ {
    case "value" => id => existsIdent(id)
    case s => _ => existsIdent(Ident(s))
  }
  //todo: more? file-exists, etc?

  def conditional2(p: Parser[Ident => MapGet[Either[String, Boolean]]]): Parser[Ident => Pattern] =
    "~[" ~> p ~ ("~;" ~> directive) ~ ("~;" ~> directive) <~ "~]" ^^ {
      case c ~ ifTrue ~ ifFalse => id => cond(c(id), ifTrue(id), ifFalse(id))
    }

  def conditional: Parser[Ident => Pattern] = conditional2(existsPred)

  def length: Parser[Ident => Pattern] = "~" ~> "[0-9]+".r ~ opt("m") ~ ("l" | "r") ~ directive ^^ {
    case num ~ None ~ pad ~ dir => id => fixedwidth(num.toInt, dir(id), pad == "l")
    case num ~ Some(_) ~ pad ~ dir => id => maxlen(num.toInt, dir(id), pad == "r")
  }

  def loopBodySimple: Parser[Ident => Pattern] =
    simpleDirective | quotedValue | sequence | conditional | length

  def combine(l: List[Ident => Pattern]): Ident => Pattern =
    id => seqq(l.map(_(id)))

  def loopBody: Parser[(Ident => Pattern, Ident => Pattern)] =
    rep1(loopBodySimple) ~ opt("~^" ~> rep1(loopBodySimple)) ^^ {
      case main ~ Some(stop) => (combine(main), combine(stop))
      case main ~ None => (combine(main), _ => empty)
    }

  def loopDirective: Parser[Ident => Pattern] = "~@" ~> opt("!") ~ opt("*") ~ ("~{" ~> loopBody <~ "~}") ^^ {
    case excl ~ all ~ body =>
      val idents = if (all.isDefined) Ident.defaults else (Ident.defaults diff VirtualProperty.idents.all)
      _ => loop(body._1, body._2, MapGet.idents(all.isDefined), excl.isEmpty)
  }

  def directive: Parser[Ident => Pattern] = simpleDirective | quotedValue | sequence | conditional | length | loopDirective

  def controlString: Parser[Pattern] = rep1(directive) ^^ { list => seqq(list.map(_('value))) }

  def parsePattern(str: String): Either[String, Pattern] =
    scala.util.Try(parseAll(controlString, str)).toEither match {
      case Right(Success(cs, _)) => Right(cs)
      case Right(f) => Left("Invalid format string!\n" + f.toString)
      case Left(m) => Left(m)
    }
}
