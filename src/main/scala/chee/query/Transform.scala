package chee.query

import chee.Collection
import chee.metadata.{ MetadataFile, MetadataMacro }
import chee.properties._
import fastparse.all._
import chee.util.parsing._

trait Transform extends (Condition => Condition) { self =>
  def comps: Set[Comp] = Set.empty

  final def ~>(next: Transform): Transform = new Transform {
    def apply(c: Condition) = (self andThen next)(c)
    override def comps = self.comps ++ next.comps
  }
}

object Transform {
  val empty = new Transform {
    def apply(c: Condition): Condition = c
    override val comps = Set.empty[Comp]
  }

  def standard(now: LocalDateTime): Transform =
    new RangeMacro(now) ~> DateMacro ~> IdMacro

  def makeChain(now: LocalDateTime, mf: MetadataFile = MetadataFile.empty) = {
    PrefixIdentTransform.default ~> new MetadataMacro(mf) ~> standard(now)
  }

  def withCollectionMacro(now: LocalDateTime, colls: Seq[Collection], mf: MetadataFile = MetadataFile.empty): Transform =
    PrefixIdentTransform.default ~> new MetadataMacro(mf) ~> new CollectionMacro(colls, mf) ~> standard(now)
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

  val default = PrefixIdentTransform(IdMacro.ident :: DateMacro.ident :: CollectionMacro.ident :: Ident.defaults ++ MetadataMacro.metaIdents)
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
        case Right(s) =>
          Junc(Junc.Or, s.map(v => Prop(Comp.Like, id -> v)).toList)
        case Left(msg) =>
          chee.UserError(msg)
      }
    case n => n
  })(c)

  object EnumParser {
    val item: P[String] = P(CharNotIn(Seq(';')).rep(1).!)
    val enum: P[Seq[String]] = P(item.rep(1, ";"))
    def parse(str: String): Either[String, Seq[String]] =
      enum.parseAll(str)
  }
}

class RangeMacro(now: LocalDateTime)  extends Transform {
  private val comp = Comp("/")
  override val comps = Set(comp)

  private val parser = new SimpleRangeParser(now)

  def apply(c: Condition) = Condition.mapAll({
    case p@Prop(`comp`, Property(id, value)) =>
      parser.parse(value) match {
        case Right((a, b)) =>
          Condition.and(Prop(Comp.Gt, id -> a), Prop(Comp.Lt, id -> b))
        case Left(msg) =>
          chee.UserError(msg)
      }
    case n => n
  })(c)

  class SimpleRangeParser(now: LocalDateTime) extends LocalDateTimeRangeParser(now) {
    val nonDash: P[String] = P(!"-" ~ AnyChar).rep(1).!

    val someRange: P[(String, String)] = P(nonDash ~ ("-"|"--") ~ nonDash)

    val stringDateRange: Parser[(String, String)] = dateRange.map {
      case DateRange(s, e) => (s.asString, e.asString)
    }

    val range: Parser[(String, String)] = stringDateRange | someRange
    def parse(str: String): Either[String, (String, String)] =
      range.parseAll(str)
  }
}

final class CollectionMacro(colls: Seq[Collection], mf: MetadataFile) extends Transform {

  import CollectionMacro._

  val query = Query.create(QuerySettings(Comp.all ++ EnumMacro.comps, Transform.makeChain(LocalDateTime.now, mf) ~> this))

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
