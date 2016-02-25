package chee.properties

case class Ident(name: String) {
  require(Ident.validate(name), s"invalid identifier: $name")

  def in(ns: String): Ident = {
    require(ns matches "[a-zA-Z][a-zA-Z0-9_]+", s"Invalid namespace name: $ns")
    Ident(ns +"-"+ name)
  }
}

object Ident {
  import scala.language.implicitConversions

  val identRegex = "[a-zA-Z][a-zA-Z0-9_\\-]*"

  val checksum: Ident = 'checksum
  val location: Ident = 'location
  val path: Ident = 'path
  val filename: Ident = 'filename
  val extension: Ident = 'extension
  val length: Ident = 'length
  val lastModified: Ident = 'lastmodified
  val added: Ident = 'added
  val created: Ident = 'created
  val make: Ident = 'make
  val model: Ident = 'model
  val width: Ident = 'width
  val height: Ident = 'height
  val orientation: Ident = 'orientation
  val iso: Ident = 'iso
  val mimetype: Ident = 'mimetype

  lazy val fileProperties = List(
    Ident.path, Ident.filename, Ident.length,
    Ident.lastModified, Ident.mimetype, Ident.extension,
    Ident.added, Ident.checksum, Ident.location)

  lazy val imageProperties = List(
    Ident.make, Ident.model, Ident.width, Ident.height,
    Ident.iso, Ident.orientation, Ident.created)

  lazy val defaults: List[Ident] =
    fileProperties ::: imageProperties ::: VirtualProperty.idents.all

  def validate(s: String): Boolean = s matches identRegex

  def isDefault(id: Ident): Boolean = defaults.exists(_ == id)

  def findIdent(all: Set[Ident], id: Ident): Either[String, Ident] = {
    val cand = all.filter(_.name startsWith id.name).toList
    cand match {
      case id :: Nil => Right(id)
      case _ :: _ :: xs =>
        Left(s"""Identifier `${id.name}' is ambiguous. Possible candidates are: ${cand.map(_.name).mkString(", ")}.""")
      case Nil =>
        Left(s"""Unknown identifier `${id.name}'!""")
    }
  }

  def sort(ids: Set[Ident]): Seq[Ident] = {
    def loop(result: List[Ident],
      list: List[Ident] = defaults,
      rem: Set[Ident] = ids): List[Ident] = list match {
      case Nil => result.reverse ::: rem.toList
      case i :: is if ids(i) => loop(i :: result, is,  rem - i)
      case i :: is => loop(result, is, rem)
    }
    loop(Nil)
  }

  implicit def string2Ident(s: String): Ident = Ident(s)
  implicit def symbol2Ident(s: Symbol): Ident = Ident(s.name)
}
