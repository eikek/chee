package chee.properties

import fastparse.all._
import chee.util.parsing._
import Patterns._
import scala.util.{ Success, Try }

/** The Parser[Ident => Pattern] is only needed for the loop body, but
  * declared for all parsers to reuse them in the loop body.
  *
  * The placeholder (value) must be replaced by the identifier of the
  * current iteration. This is done by providing it to the
  * function. For the special identifier `value` (only valid inside
  * the loop), the identifier provided to the function is used,
  * otherwise the parsed identifier is used.
  */
final class PatternParser(idents: Traversable[Ident]) {

  def lift(p: P[Pattern]): P[Ident => Pattern] = p.map(p => _ => p)

  val tilde: P[String] = P("~" ~ "~".!)
  val noTilde: P[String] = P(CharsWhile(_ != '~').!)

  val rawValue: P[Pattern] = P((tilde | noTilde).rep(1)).map {
    list => seq(list.map(raw(_)))
  }

  val lookupValue: P[Ident => Pattern] =
    P("~#" ~ identString.! ~ ("~f" ~ noTilde.!).?).map {
      case ("value", format) => id => lookup(id, format)
      case (id, format) => _ => lookup(Ident(id), format)
    }

  val readableValue: P[Ident => Pattern] =
    P("~:" ~ identString.!).map {
      case "value" => id => readable(id)
      case s => _ => readable(Ident(s))
    }

  val newlineValue: P[Pattern] = P("~%").map { _ => newline }

  val emptyValue: P[Pattern] = P("~.").map { _ => empty }

  val simpleDirective: P[Ident => Pattern] =
    lift(emptyValue) | lift(newlineValue) | lookupValue | readableValue | lift(rawValue)

  def quotedValue: P[Ident => Pattern] =
    P("~" ~ ("\"" | "'").! ~ directive).map {
      case (q, directive) => id => quote(q.charAt(0), directive(id))
    }

  def sequence: P[Ident => Pattern] =
    P("~{" ~ directive.rep(1) ~ "~}").map {
      case ds => id => seq(ds.map(_(id)))
    }

  def existsPred: P[Ident => MapGet[Either[String, Boolean]]] =
    P(identString.!).map {
      case "value" => id => existsIdent(id)
      case s => _ => existsIdent(Ident(s))
    }
  //todo: more? file-exists, etc?

  def conditional2(p: P[Ident => MapGet[Either[String, Boolean]]]): P[Ident => Pattern] =
    P("~[" ~ p ~ "~;" ~ directive ~ "~;" ~ directive ~ "~]").map {
      case (c, ifTrue, ifFalse) => id => cond(c(id), ifTrue(id), ifFalse(id))
    }

  def conditional: P[Ident => Pattern] = conditional2(existsPred)

  def length: P[Ident => Pattern] =
    P("~" ~ (digit.rep(1)).! ~ ("m".!).? ~ ("l" | "r").! ~ directive).map {
      case (num, None, pad, dir) => id => fixedwidth(num.toInt, dir(id), pad == "l")
      case (num, Some(_), pad, dir) => id => maxlen(num.toInt, dir(id), pad == "r")
    }

  def loopBodySimple: P[Ident => Pattern] =
    simpleDirective | quotedValue | sequence | conditional | length

  def combine(l: Seq[Ident => Pattern]): Ident => Pattern =
    id => seq(l.map(_(id)))

  def loopBody: P[(Ident => Pattern, Ident => Pattern)] =
    P(loopBodySimple.rep(1) ~ ("~^" ~ loopBodySimple.rep(1)).?).map {
      case (main, Some(stop)) => (combine(main), combine(stop))
      case (main, None) => (combine(main), _ => empty)
    }

  def loopDirective: Parser[Ident => Pattern] =
    P("~@" ~ ("!".!.?) ~ ("*".!.?) ~ "~{" ~ loopBody ~ "~}").map {
      case (excl, all, body) =>
        _ => loop(body._1, body._2, MapGet.idents(all.isDefined), excl.isEmpty)
    }

  def directive: P[Ident => Pattern] =
    simpleDirective | quotedValue | sequence | conditional | length | loopDirective

  def controlString: P[Pattern] =
    P(directive.rep(1)).map { list => seq(list.map(_('value))) }

  def parsePattern(str: String): Either[String, Pattern] =
    Try(controlString.parseAll(str)) match {
      case Success(p) => p
      case f =>  Left("Invalid format string!\n" + f.toString)
    }
    // scala.util.Try(parseAll(controlString, str)).toEither match {
    //   case Right(Success(cs, _)) => Right(cs)
    //   case Right(f) => Left("Invalid format string!\n" + f.toString)
    //   case Left(m) => Left(m)
    // }
}
