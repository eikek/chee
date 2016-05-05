package chee.metadata

import better.files.File
import chee.UserError
import chee.properties._
import chee.query._
import RecFormat._
import MapGet._

trait MetadataFile {

  def findIds(c: Condition): Traversable[String] = {
    val id = MapGet.valueForce(Ident.checksum)
    find(c).map(id.result)
  }

  /** Search the metadata file using the given condition.
    * 
    * The condition must only search for {{tag}}, {{comment}} and
    * {{checksum}} properties, since there are no more in the
    * metadata. */
  def find(c: Condition): Traversable[LazyMap]

  /** Write new data to the metadata file.
    * 
    * The given data maps must contain at least the {{checksum}}
    * property. Additional the {{tag}} and {{comment}} properties are
    * read and the corresponding entry is updated in the metadata
    * file. */
  def write(data: Stream[LazyMap]): MetadataFile
}

object MetadataFile {

  def apply(f: File): MetadataFile = new Impl(f)

  def parseCondition(q: String): Either[String, Condition] = 
    queryParser(q)
  
  val idents = Set(
    Ident.checksum,
    Ident.tag,
    Ident.comment)

  private val descriptor = Descriptor (
    Field("%rec", "chee-metadata", 0),
    Field("%key", "Checksum", 0),
    Field("%unique", "Comment", 0),
    Field("%type", "Checksum regexp /[a-f0-9]+/", 0),
    Field("%type", "Tag regexp /[^\\s,;'\"]+/", 0)
  )

  object TagValueTransform extends Transform {
    def apply(c: Condition): Condition = Condition.mapAll({
      case Prop(comp, Property(Ident.tag, value)) =>
        Prop(comp, Property(Ident.tag, "*["+value+"]*"))
      case n => n
    })(c)
  }

  private val queryTransform =
    new PrefixIdentTransform(idents) ~> TagValueTransform ~> EnumMacro ~> IdMacro 

  private val queryParser = Query.create(QuerySettings(Comp.all, queryTransform))

  private def updateFn: MapGet[(String, Record => Record)] =
    pair(valueForce(Ident.checksum), pair(value(Ident.tag), value(Ident.comment))).map {
      case (id, (tag, comment)) =>
        val f: Record => Record = ???
        (id, f)
    }

  private class Impl(f: File) extends MetadataFile {
    val parser = new MapParser()
    val dbParser = new DatabaseParser()
    // read the file once, subsequent finds can reuse the result
    lazy val results = parser.parseFile(f) match {
      case Right(r) => r
      case Left(msg) => UserError(s"The metadata file contains errors: $msg")
    }

    def find(c: Condition): Traversable[LazyMap] = {
      val p = Predicates(queryTransform(c))
      MapGet.filter(results, p)
    }    

    def write(data: Stream[LazyMap]): MetadataFile = {
      // load recfile
      val db = dbParser.parseFile(f) match {
        case Right(x) => x
        case Left(msg) => UserError(s"The metadata file contains errors: $msg")
      }
      // apply changes from map
      val updates: Map[String, Record => Record] =
        data.map(updateFn.result).toMap
      val newDb = db.mapPf {
        case RecordId(id, r) if updates contains id =>
          updates(id)(r)
      }
      // write back to file (new file + move; make a utility for that in files)
      "" `>:` f
      ???
      // return new instance (clears search data)
      new Impl(f)
    }

  }
}
