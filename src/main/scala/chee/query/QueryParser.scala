package chee.query

import chee.util.parsing._
import chee.properties._

class QueryParser(comparators: Set[Comp]) {
  import fastparse.noApi._
  import IgnoreWhitespace._

  lazy val comp: P[Comp] =
    P(comparators.map(c => ic(c.name).!).reduce(_ | _)).map(Comp.apply)

  lazy val ident: P[Ident] = identString.!.map(Ident.apply)

  lazy val propValue: P[String] = P(
    QueryParser.quotedValue | QueryParser.simpleValue
  )

  lazy val prop: P[Prop] = P(ident ~ comp ~ propValue).map {
    case (id, c, v) => Prop(c, id -> v)
  }

  lazy val idprop: P[IdentProp] = P(ident ~ comp ~ ("'" ~ ident)).map {
    case (id1, c, id2) => IdentProp(c, id1, id2)
  }

  lazy val exists: P[Exists] = P(ident ~ "?").map(Exists.apply)

  lazy val juncOp: P[Junc.Op] = P("&" | "|").!.map {
    case "&" => Junc.And
    case _ => Junc.Or
  }

  lazy val not: P[Not] = P("!" ~ condition).map(Not.apply)

  lazy val junc: P[Junc] =
    P("(" ~ juncOp ~ condition.rep(1) ~ ")").map {
      case (op, conds) => Junc(op, conds.toList)
    }

  lazy val condition: P[Condition] =
    junc | not | idprop | prop | exists

  def parse(in: String): Either[String, Condition] =
    condition.parseAll(in)
}

object QueryParser {
  import fastparse.all._

  lazy val quotedValue: P[String] = P(quotedString('"') | quotedString('''))
  lazy val simpleValue: P[String] = P(CharNotIn(Seq(')', '"', ''', ' ', '\t')).rep.!)

  private lazy val defaultParser = new QueryParser(Comp.all)

  def parse(in: String, comps: Set[Comp] = Comp.all): Either[String, Condition] =
    if (comps == Comp.all) defaultParser.parse(in)
    else new QueryParser(comps).parse(in)
}
