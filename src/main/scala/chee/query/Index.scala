package chee.query

import better.files._
import chee.metadata.MetadataFile
import chee.properties._
import scala.util.{Failure, Success, Try}
import Index._

trait Index {

  def find(cond: Condition): Try[Stream[LazyMap]] =
    find(cond, MetadataFile.empty)

  def findOne(cond: Condition, mf: MetadataFile): Try[Option[LazyMap]] =
    find(cond, mf).map(_.take(2).toList).flatMap {
      case Nil => Success(None)
      case a :: Nil => Success(Some(a))
      case _ :: _ :: _ => Failure(new RuntimeException("Too many results for `findOne'"))
    }

  def findOne(cond: Condition): Try[Option[LazyMap]] =
    findOne(cond, MetadataFile.empty)

  def find(cond: Condition, mf: MetadataFile): Try[Stream[LazyMap]]

  def listLocations(): Try[Seq[File]]

  def locationInfo(): Try[LocationInfo]

  def insert[C](data: Traversable[LazyMap], zero: C, p: Progress[Boolean, C]): Try[C]

  def insertOne: MapGet[Try[Boolean]]

  @deprecated("", "")
  def insertOne(m: LazyMap): Try[(LazyMap, Boolean)] = {
    val (next, result) = insertOne.run(m)
    result.flatMap(b => Success((next, b)))
  }

  def update[C](data: Traversable[LazyMap], param: UpdateParam = UpdateParam() , zero: C, p: Progress[Boolean, C]): Try[C]

  def updateOne(param: UpdateParam): MapGet[Try[Boolean]]

  @deprecated("", "0.2.0")
  def updateOne(m: LazyMap, param: UpdateParam = UpdateParam()): Try[(LazyMap, Boolean)] = {
    val (next, result) = updateOne(param).run(m)
    result.flatMap(b => Success((next, b)))
  }

  def count(cond: Condition): Try[Int]

  def delete(cond: Condition): Try[Int]

  def move(source: File, target: File, newLocation: Option[File]): Try[Int]

  def exists(cond: Condition): Try[Boolean] =
    count(cond).map(_ > 0)

  def exists(cond: MapGet[Condition]): MapGet[Try[Boolean]] =
    cond.map(exists)
}

object Index {

  val realPathIdent = Ident.path.in("real")

  val realPath: MapGet[Option[String]] =
    MapGet.value(realPathIdent).orElse(MapGet.value(Ident.path))

  /** Specifies what to update: columns and where condition. Default is
    * all columns and only itself (where path = x). */
  case class UpdateParam(columns: MapGet[Seq[Ident]] = MapGet.idents(false), where: MapGet[Condition] = Condition.lookup(Ident.path))

  object UpdateParam {
    def updatePathWith(valueIdent: Ident): UpdateParam =
      UpdateParam(MapGet.unit(Seq(Ident.path)), Condition.lookup(Comp.Eq, Ident.path, valueIdent))
  }

  case class LocationInfo(count: Map[File, Int])
}
