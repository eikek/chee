package chee.query

import better.files._
import chee.crypto.CheeCrypt
import chee.properties._
import chee.util.files._

// todo: despite its name SqlBackend, some queries  are sqlite specific
object SqlBackend {
  import Patterns._

  val idents = Ident.defaults diff VirtualProperty.idents.all

  def insertStatement(table: String): MapGet[String] =
    seq(
      raw("INSERT INTO "),
      raw(table),
      raw(" ("),
      loop(
        id => lookup('ident),
        id => raw(","),
        MapGet.idents(false),
        includeEmpty = false),
      raw(") VALUES ("),
      loop(
        id => cond(existsIdent(id), quote(''', lookup(id)), raw("null")),
        id => raw(","),
        MapGet.idents(false),
        includeEmpty = false),
      raw(")")).right

  def updateRowStatement(table: String, columns: MapGet[Seq[Ident]] = MapGet.idents(false), where: IdentProp = IdentProp(Comp.Eq, Ident.path, Ident.path)): MapGet[String] =
    seq(
      raw("UPDATE "), raw(table), raw(" SET "),
      loop(
        id => seq(lookup('ident), raw(" = "), cond(existsIdent(id), quote(''', lookup(id)), raw("null"))),
        id => raw(", "),
        columns.map(_.filterNot(_ == Ident.added).intersect(idents)),
        includeEmpty = true),
      raw(" WHERE "),
      raw(columnSql(where.id1, where.comp)),
      raw(" "+ operatorSql(where.comp) +" "),
      sqlValue(where)).right

  def createTable(name: String): String = {
    val cols = idents.foldLeft(List[String]()){ (sql, id) =>
      (id.name + (Value.forIdent(id) match {
        case _: IntCompare => " integer"
        case _: LongCompare => " integer"
        case _ => " text"
      })) :: sql
    }
    s"""CREATE TABLE $name (${cols.mkString(",")})"""
  }

  def whereClause(c: Condition): String = Condition.reduce[String](
    leaf => leaf match {
      case p: Prop => p.toSql
      case i: IdentProp => i.toSql
      case e: Exists => e.toSql
      case TrueCondition => "1=1"
    },
    op => (s1, s2) => s"""$s1 ${if (op == Junc.Or) "or" else "and"} $s2""",
    op => ob => ob.map(c => s"($c)").getOrElse(sys.error("Invalid tree: Cannot map an empty junction to a predicate.")),
    p => s"not($p)"
  )(c)

  def deleteStatement(table: String, c: Condition): String =
    s"DELETE FROM $table WHERE " + whereClause(c)

  def count(table: String, c: Condition): String =
    s"SELECT COUNT(*) FROM $table WHERE ${whereClause(c)}"

  def move(source: File, target: File, newLocation: Option[File]): String = {
    val len = source.toString.length + 1
    val updates = newLocation.map(nl => s"location = '${nl.path}'").toList ++ (source match {
      case RegularFile(_) => List(
        s"path = '${target.path}'",
        s"filename = '${target.name}'",
        s"""extension = ${target.getExtension.map("'" +_+ "'").getOrElse("null")}""")
      case _ =>
        List(s"path = '${target.path}' || substr(path, $len)")
    })
    s"""UPDATE chee_index SET ${updates.mkString(", ")} WHERE path like '${source.path}%'"""
  }


  private val pbc = CheeCrypt.passwordEncryptExtension
  private val pkc = CheeCrypt.publicKeyEncryptExtension
  private val encryptedExpr =
    s"(case when substr(${Ident.path.name}, -4) = '.$pbc' then '$pbc' when substr(${Ident.path.name}, -4) = '.$pkc' then '$pkc' else null end)"

  private def columnSql(id: Ident, comp: Comp): String = id match {
    case VirtualProperty.idents.pixel =>
      s"(${Ident.width.name} * ${Ident.height.name})"
    case VirtualProperty.idents.encrypted =>
      encryptedExpr
    case Ident.added if comp == Comp.Like => "datetime(added/1000, 'unixepoch')"
    case Ident.lastModified if comp == Comp.Like => "datetime(lastmodified/1000, 'unixepoch')"
    case Ident.created if comp != Comp.Like => "datetime(created)"
    case id => id.name
  }

  private def operatorSql(comp: Comp) = comp match {
      case Comp.Like => "like"
      case c => c.name
    }

  private def sqlValue(prop: IdentProp): Pattern =
    lookup(prop.id2).rmap { value =>
      Prop(prop.comp, prop.id1 -> value).sqlValue
    }


  private implicit class PropSql(p: Prop) {

    def parse(v: Value[_], s: String, comp: Comp, id: Ident): String = v.parse(s) match {
      case Right(x) => x.toString
      case Left(m) => chee.UserError(s"Cannot use `$s' to compare via `${comp.name}' a ${id.name} value.")
    }

    private def localDateTimeValue(s: String) =
      LocalDateTimeValue.parse(s) match {
        case Right(dt) => s"datetime(${dt.toDateTime().instant / 1000}, 'unixepoch')"
        case Left(m) => chee.UserError(s"Cannot create a date/time value from `$s': $m")
      }

    def sqlValue = {
      def quote(s: String) = s"'$s'"

      if (p.comp == Comp.Like) quote(p.prop.value.replace("*", "%"))
      else Value.forIdent(p.prop.ident) match {
        case c: IntCompare => parse(c, p.prop.value, p.comp, p.prop.ident)
        case c: LongCompare => parse(c, p.prop.value, p.comp, p.prop.ident)
        case LocalDateTimeValue => localDateTimeValue(p.prop.value)
        case _ => quote(p.prop.value)
      }
    }

    def toSql: String =
      s"${columnSql(p.ident, p.comp)} ${operatorSql(p.comp)} ${sqlValue}"
  }

  implicit class IdentPropSql(p: IdentProp) {
    def toSql: String =
      s"${columnSql(p.id1, p.comp)} ${operatorSql(p.comp)} ${columnSql(p.id2, p.comp)} "
  }

  implicit class ExistsSql(e: Exists) {
    def toSql: String = e.ident match {
      case VirtualProperty.idents.pixel =>
        s"(${Ident.width.name} is not null and ${Ident.height.name} is not null)"
      case VirtualProperty.idents.encrypted =>
        s"$encryptedExpr is not null"
      case _ =>
        s"${e.ident.name} is not null"
    }
  }
}
