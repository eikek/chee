package chee.query

import chee.util.parsing._
import chee.properties._

class QueryParser(comparators: Set[Comp]) {
  import fastparse.all._
//  import IgnoreWhitespace._

  lazy val comp: P[Comp] =
    P(StringIn(comparators.map(_.name).toSeq: _*).!.map(Comp.apply))

  lazy val ident: P[Ident] = identString.!.map(Ident.apply)

  lazy val propValue: P[String] = P(
    QueryParser.quotedValue | QueryParser.simpleValue
  )

  lazy val prop: P[Prop] = P(ident ~ WS.rep ~ comp ~/ WS.rep ~ propValue).map {
    case (id, c, v) => Prop(c, id -> v)
  }

  lazy val idprop: P[IdentProp] = P(ident ~ WS.rep ~ comp ~ WS.rep ~ "'" ~ ident ~ !"'").map {
    case (id1, c, id2) => IdentProp(c, id1, id2)
  }

  lazy val exists: P[Exists] = P(ident ~ WS.rep ~ "?").map(Exists.apply)

  def simpleEnum(stopChars: Seq[Char]): P[Seq[String]] =
    P(stringEscape('\\', stopChars :+ ';').rep(1, sep = ";"))

  def quotedEnum(quote: Char): P[Seq[String]] = P(
    quote.toString ~ simpleEnum(Seq(quote)) ~ quote.toString
  )

  lazy val in: P[In] = P(ident ~ WS.rep ~ "~" ~ WS.rep ~/ (quotedEnum('\'') | quotedEnum('"') | simpleEnum(")\"' \t"))).map {
    case (id, values) => In(id, values)
  }

  lazy val juncOp: P[Junc.Op] = P("&" | "|").!.map {
    case "&" => Junc.And
    case _ => Junc.Or
  }

  lazy val not: P[Not] = P("!" ~ WS.rep ~ condition).map(Not.apply)

  lazy val junc: P[Junc] =
    P("(" ~ WS.rep ~ juncOp ~/ WS.rep ~ condition.rep(1) ~ ")").map {
      case (op, conds) => Junc(op, conds.toList)
    }

  lazy val condition: P[Condition] =
    P(WS.rep ~ (junc | not | in | idprop | prop | exists) ~ WS.rep)

  def parse(in: String): Either[String, Condition] =
    condition.parseAll(in)
}

object QueryParser {
  import fastparse.all._

  lazy val quotedValue: P[String] = P(quotedString('"') | quotedString('\''))
  lazy val simpleValue: P[String] = P(CharNotIn(Seq(')', '"', '\'', ' ', '\t')).rep.!)

  private lazy val defaultParser = new QueryParser(Comp.all)

  def parse(in: String, comps: Set[Comp] = Comp.all): Either[String, Condition] =
    if (comps == Comp.all) defaultParser.parse(in)
    else new QueryParser(comps).parse(in)
}
