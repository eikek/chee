package chee.metadata

import better.files.File
import chee.UserError
import chee.properties._
import chee.query._
import chee.util.files._
import chee.util.predicates._
import RecElement._
import RecFormat._
import com.typesafe.scalalogging.LazyLogging

trait MetadataFile {
  def findIds(c: Condition): Traversable[String] = {
    val id = MapGet.valueForce(Ident.checksum)
    find(c).map(id.result)
  }

  def query(q: String): Either[String, Traversable[LazyMap]] =
    MetadataFile.parseCondition(q).right.map(find)

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
  def write(data: Traversable[LazyMap]): MetadataFile

  /** Delete all records matching the checksum of the given data. */
  def delete(data: Traversable[LazyMap]): MetadataFile
}

object MetadataFile {
  val empty = new MetadataFile {
    def find(c: Condition): Traversable[LazyMap] = Seq.empty
    def write(data: Traversable[LazyMap]): MetadataFile = this
    def delete(data: Traversable[LazyMap]): MetadataFile = this
  }

  def apply(f: File): MetadataFile = new Impl(f)

  def parseCondition(q: String): Either[String, Condition] = 
    queryParser.parse(q)

  object TagValueTransform extends Transform {
    def apply(c: Condition): Condition = Condition.mapAll({
      case Prop(comp, Property(idents.tag, value)) =>
        Prop(comp, Property(idents.tag, s"*${Tag.separator}${value}${Tag.separator}*"))
      case n => n
    })(c)
  }

  private val queryTransform =
    new PrefixIdentTransform(idents.all.toSet + Ident.checksum) ~>
    TagValueTransform ~> EnumMacro ~> IdMacro

  private val queryParser = Query.create(QuerySettings(Comp.all, queryTransform))


  private val newDatabase = Database(Vector(Descriptor(
    Field("%rec", "chee-metadata", 0),
    Field("%key", "Checksum", 0),
    Field("%unique", "Comment", 0),
    Field("%type", "Checksum regexp /[a-f0-9]+/", 0),
    Field("%type", s"Tag regexp /${Tag.tagRegex}/", 0)
  )))


  private class Impl(f: File) extends MetadataFile with LazyLogging {
    val parser = new MapParser()
    val dbParser = new DatabaseParser()
    // read the file once, subsequent finds can reuse the result
    lazy val results = parse(parser.parseFile).getOrElse(Seq.empty)

    private def parse[A](p: File => Either[String, A]): Option[A] =
      f.existing.map(p(_) match {
        case Right(x) => x
        case Left(msg) => UserError(s"The metadata file contains errors: $msg")
      })

    def find(c: Condition): Traversable[LazyMap] = {
      logger.trace(s"Search metadata for: $c")
      val p = Predicates(queryTransform(c))
      MapGet.filter(results, p)
    }    

    def write(data: Traversable[LazyMap]): MetadataFile = {
      // build up new data
      val (ids, records) = 
        MapGet.fold((Set.empty[String], Vector.empty[Record]), data) {
          case (ids, records) =>
            mapget.idAndRecord.map { case (id, rec) =>
              if (ids(id)) (ids, records)
              else (ids + id, records :+ rec)
            }
        }
      // load recfile
      val db = parse(dbParser.parseFile) getOrElse newDatabase
      // keep only those not in data and append data
      val newDb = db.filterId(not(ids)) ++ Database(records)

      // write back to file
      newDb.render ==>>: f 
      // return new instance (clears search data)
      new Impl(f)
    }

    def delete(data: Traversable[LazyMap]): MetadataFile = {
      logger.trace(s"Delete metadata …")
      val ids = MapGet.fold(Set.empty[String], data) {
        ids => MapGet.valueForce(Ident.checksum).map(ids + _)
      }
      // load recfile
      val db = parse(dbParser.parseFile) getOrElse newDatabase
      // keep only those not in data
      val newDb = db.filterId(not(ids))
      newDb.render ==>>: f
      new Impl(f)
    }
  }
}
