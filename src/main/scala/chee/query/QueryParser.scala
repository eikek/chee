package chee.query

import scala.util.parsing.combinator.RegexParsers
import chee.properties._

object QueryParser extends RegexParsers {

  val ident: Parser[Ident] = Ident.identRegex.r ^^ (id => Ident(id))

  def comp(all: Set[Comp]): Parser[Comp] =
    all.map(c => literal(c.name)).reduce(_ | _).map(c => Comp(c))

  val simpleValue: Parser[String] = """[^\s\\)"']+""".r

  private def escapedChar(escapeChar: Char, terminalChars: Set[Char]): Parser[Char] = Parser { in =>
    if (in.atEnd) Failure("empty input", in)
    else {
      val fchar = in.first
      if (fchar == escapeChar) {
        val fin = in.drop(1)
        if (fin.atEnd) Failure("escape char at end", in.rest)
        else Success(fin.first, fin.rest)
      } else {
        if (terminalChars contains fchar) Failure("end of input sequence", in)
        else Success(fchar, in.rest)
      }
    }
  }

  private def quotedChars(quoteChar: Char): Parser[String] =
    quoteChar ~ rep(escapedChar('\\', Set(quoteChar))) ~ quoteChar ^^ {
      case _ ~ seq ~ _ => seq.mkString
    }

  val quotedValue: Parser[String] = quotedChars('"') | quotedChars(''')

  val propValue: Parser[String] = quotedValue | simpleValue

  def prop(all: Set[Comp]): Parser[Prop] = ident ~ comp(all) ~ propValue ^^ {
    case id ~ op ~ value => Prop(op, id -> value)
  }

  def exists: Parser[Exists] = ident <~ "?" ^^ {
    case name => Exists(name)
  }

  def not(all: Set[Comp]): Parser[Not] = "!" ~> condition(all) ^^ {
    case cond => Not(cond)
  }

  def juncOp: Parser[Junc.Op] = accept("Expected & or |", {
    case '&' => Junc.And
    case '|' => Junc.Or
  })

  def junc(all: Set[Comp]): Parser[Junc] = "(" ~> juncOp ~ rep1(condition(all)) <~ ")" ^^ {
    case op ~ conds => Junc(op, conds)
  }

  def condition(all: Set[Comp]): Parser[Condition] =
    junc(all) | not(all) | prop(all) | exists

  def apply(in: String, comps: Set[Comp] = Comp.all): Either[String, Condition] =
    parseAll(condition(comps), in.trim) match {
      case Success(v, _) => Right(v)
      case f => Left(f.toString)
    }
}
