package chee.query

import better.files._
import chee.properties._
import chee.util.files._
import java.nio.file.Path
import Patterns._
import SqlFormat._

// todo: despite its name SqlBackend, some queries  are sqlite specific
object SqlBackend {

  val idents = Ident.defaults diff VirtualProperty.idents.all

  private def valuePattern(id: Ident) = {
    def ofId(id: Ident) = cond(existsIdent(id), quote(''', lookup(id)), raw("null"))
    if (id == Ident.path) cond(existsIdent(Index.realPathIdent), ofId(Index.realPathIdent), ofId(id))
    else ofId(id)
  }

  def insertStatement(table: String): MapGet[String] =
    seq(
      raw("INSERT INTO "),
      raw(table),
      raw(" ("),
      loop(
        id => lookup('ident),
        id => raw(","),
        MapGet.unit(idents),
        includeEmpty = false),
      raw(") VALUES ("),
      loop(
        id => valuePattern(id),
        id => raw(","),
        MapGet.unit(idents),
        includeEmpty = false),
      raw(")")).right

  def updateRowStatement(table: String, columns: MapGet[Seq[Ident]] = MapGet.idents(false), where: MapGet[Condition] = Condition.lookup(Ident.path)): MapGet[String] =
    seq(
      raw("UPDATE "), raw(table), raw(" SET "),
      loop(
        id => seq(lookup('ident), raw(" = "), valuePattern(id)),
        id => raw(", "),
        columns.map(_.filterNot(_ == Ident.added).intersect(idents)),
        includeEmpty = true),
      raw(" WHERE "),
      where.flatMap(c => raw(render(c)))).right

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

  def whereClause(c: Condition): String =
    SqlFormat.render(c)

  def deleteStatement(table: String, c: Condition): String =
    s"DELETE FROM $table WHERE " + whereClause(c)

  def count(table: String, c: Condition): String =
    s"SELECT COUNT(*) FROM $table WHERE ${whereClause(c)}"

  def move(source: Path, target: Path, isRegularFile: Boolean, newLocation: Option[Path]): String = {
    val len = source.toString.length + 1
    val t: File = target
    val updates = newLocation.map(nl => s"location = '$nl'").toList ++ {
      if (isRegularFile) List(
        s"path = '$target'",
        s"filename = '${t.name}'",
        s"""extension = ${t.getExtension.map("'" +_+ "'").getOrElse("null")}""")
      else
        List(s"path = '${target}' || substr(path, $len)")
    }
    s"""UPDATE chee_index SET ${updates.mkString(", ")} WHERE path like '${source}%'"""
  }
}
