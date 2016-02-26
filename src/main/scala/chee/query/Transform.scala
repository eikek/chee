package chee.query

import chee.Collection
import chee.properties._

trait Transform extends (Condition => Condition) { self =>
  def comps: Set[Comp] = Set.empty

  final def ~>(next: Transform): Transform = new Transform {
    def apply(c: Condition) = (self andThen next)(c)
    override def comps = self.comps ++ next.comps
  }
}

object Transform {
  def makeChain(now: LocalDateTime) = {
    PrefixIdentTransform.default ~>
    new RangeMacro(now) ~> EnumMacro ~> DateMacro ~> IdMacro
  }

  def withCollectionMacro(colls: Seq[Collection]): Transform =
    PrefixIdentTransform.default ~> new CollectionMacro(colls) ~>
    new RangeMacro(LocalDateTime.now) ~> EnumMacro ~> DateMacro ~> IdMacro
}

final class PrefixIdentTransform(val idents: Set[Ident]) extends Transform {
  def apply(c: Condition): Condition = Condition.mapAll({
    case Prop(comp, Property(ident, value)) =>
      Prop(comp, Property(findIdent(ident), value))
    case IdentProp(comp, id1, id2) =>
      IdentProp(comp, findIdent(id1), findIdent(id2))
    case Exists(ident) =>
      Exists(findIdent(ident))
    case n => n
  })(c)

  def findIdent(ident: Ident): Ident =
    Ident.findIdent(idents, ident) match {
      case Right(id) => id
      case Left(msg) => chee.UserError(msg)
    }
}

object PrefixIdentTransform {
  def apply(idents: Iterable[Ident]) = new PrefixIdentTransform(idents.toSet)

  val default = PrefixIdentTransform(IdMacro.ident :: DateMacro.ident :: CollectionMacro.ident :: Ident.defaults)
}

object DateMacro extends Transform {
  val ident: Ident = 'date

  def apply(c: Condition) = Condition.mapAll({
    case Prop(comp, Property(`ident`, value)) =>
      Condition.or(
        Prop(comp, Ident.created -> value),
        Condition.and(Not(Exists(Ident.created)), Prop(comp, Ident.lastModified -> value)))
    case IdentProp(_, _, `ident`) =>
      chee.UserError(s"Cannot use `${ident.name}' as a right hand side")
    case IdentProp(comp, `ident`, valueId) =>
      Condition.or(
        IdentProp(comp, Ident.created, valueId),
        Condition.and(Not(Exists(Ident.created)), IdentProp(comp, Ident.lastModified, valueId)))
    case Exists(`ident`) =>
      Condition.or(Exists(Ident.created), Exists(Ident.lastModified))
    case n => n
  })(c)
}

object IdMacro extends Transform {
  val ident: Ident = 'id

  def apply(c: Condition) = Condition.mapAll({
    case Prop(comp, Property(`ident`, value)) =>
      if (value.length == 64) Prop(Comp.Eq, Ident.checksum -> value)
      else if (value.length > 64) chee.UserError(s"Invalid id value: ${value}. Must be <= 64 characters.")
      else Prop(Comp.Like, Ident.checksum -> s"${value}*")
    case IdentProp(comp, id1, id2) =>
      IdentProp(comp,
        if (id1 == ident) Ident.checksum else id1,
        if (id2 == ident) Ident.checksum else id2)
    case Exists(`ident`) =>
      Exists(Ident.checksum)
    case n => n
  })(c)
}

object EnumMacro extends Transform {
  private val comp = Comp("~")
  override val comps = Set(comp)

  def apply(c: Condition) = Condition.mapAll({
    case p@Prop(`comp`, Property(id, value)) =>
      EnumParser.parse(value) match {
        case Right(list) =>
          Junc(Junc.Or, list.map(v => Prop(Comp.Like, id -> v)))
        case Left(msg) =>
          chee.UserError(msg)
      }
    case n => n
  })(c)

  object EnumParser extends scala.util.parsing.combinator.RegexParsers {
    override val whiteSpace = "".r
    val item: Parser[String] = "[^;]+".r
    val enum: Parser[List[String]] = (item <~ ";") ~ rep1sep(item, ";") ^^ {
      case first ~ rest => first :: rest
    }
    def parse(str: String): Either[String, List[String]] =
      parseAll(enum, str.trim) match {
        case Success(l, _) => Right(l)
        case f => Left(f.toString)
      }
  }
}

class RangeMacro(now: LocalDateTime)  extends Transform {
  private val comp = Comp("/")
  override val comps = Set(comp)

  private val parser = new SimpleRangeParser(now)

  def apply(c: Condition) = Condition.mapAll({
    case p@Prop(`comp`, Property(id, value)) =>
      parser.parseRange(value) match {
        case Right((a, b)) =>
          Condition.and(Prop(Comp.Gt, id -> a), Prop(Comp.Lt, id -> b))
        case Left(msg) =>
          chee.UserError(msg)
      }
    case n => n
  })(c)

  class SimpleRangeParser(now: LocalDateTime) extends DateTimeParser(now) {
    val someRange: Parser[(String, String)] = "[^\\-]+".r ~ (("-"|"--") ~> "[^\\-]+".r) ^^ {
      case s1 ~ s2 => (s1, s2)
    }

    val stringDateRange: Parser[(String, String)] = dateRange.map {
      case DateRange(s, e) => (s.asString, e.asString)
    }

    val range: Parser[(String, String)] = stringDateRange | someRange
    def parseRange(str: String): Either[String, (String, String)] =
      parseAll(range, str.trim) match {
        case Success(r, _) => Right(r)
        case f => Left(f.toString)
      }
  }
}

final class CollectionMacro(colls: Seq[Collection]) extends Transform {

  import CollectionMacro._

  val query = Query.create(QuerySettings(Comp.all, Transform.makeChain(LocalDateTime.now) ~> this))

  lazy val queryMap = colls.map(c => c.name -> c.query).toMap

  def apply(c: Condition) = Condition.mapAll({
    case Prop(_, Property(`ident`, value)) =>
      queryMap.get(value) match {
        case Some(q) => query(q) match {
          case Right(c) => c
          case Left(m) => chee.UserError(m)
        }
        case None =>
          chee.UserError(s"Collection not found: `$value'")
      }
    case IdentProp(_, id1, id2) if id1 == ident || id2 == ident =>
      chee.UserError(s"Cannot compare `${ident.name}' to each other")
    case Exists(`ident`) =>
      chee.UserError(s"Cannot use `${ident.name}' in exists")
    case n => n
  })(c)
}

object CollectionMacro {
  val ident: Ident = 'collection
}
