package chee.query

import java.sql._

import scala.util.{Failure, Success, Try}

import better.files.File
import chee.Timing
import chee.conf._
import chee.metadata.MetadataFile
import chee.properties._
import chee.util.paths
import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import Index.{LocationInfo, UpdateParam}

class SqliteBackend(dbfile: File, root: Option[File], pageSize: Int = 500) extends Index with JdbcConnection with LazyLogging {
  import SqliteBackend._
  def this(cfg: Config) = this(cfg.getIndexDb, cfg.getRepoRoot)

  dbfile.parent.createDirectories()

  val jdbcUrl = s"jdbc:sqlite:${dbfile.path}"

  final def find(cond: Condition, mf: MetadataFile): Try[Stream[LazyMap]] = {
    val c = root.map(relativeCond(_)(cond)) getOrElse cond
    val sql = (s"""SELECT ${SqlBackend.idents.map(_.name).mkString(",")},coalesce(created,lastmodified) as sorting"""
      + s""" FROM chee_index WHERE ${SqlBackend.whereClause(c)}"""
      + s""" ORDER BY sorting""")
    val files = findNext(sql, 0, mf).recoverWith {
      case ex => Failure(new RuntimeException(s"Error for sql: $sql", ex))
    }
    files.map(resolveTo(root))
  }

  private final def findNext(sql: String, skip: Int, mf: MetadataFile): Try[Stream[LazyMap]] = {
    val first = withConn(dbfile) { implicit conn =>
      val buffer = collection.mutable.ListBuffer[LazyMap]()
      val rs = sqlQuery(sql + s" limit $skip,$pageSize")
      while (rs.next) {
        buffer += rs.toPropertyMap(root, mf)
      }
      buffer.toStream
    }
    Try {
      val f = first.get
      if (f.isEmpty) f
      else f #::: findNext(sql, skip+pageSize, mf).get
    }
  }

  def listLocations(): Try[Seq[File]] =
    withConn(dbfile) { implicit conn =>
      val buffer = collection.mutable.ListBuffer[File]()
      val rs = sqlQuery(s"select distinct(${Ident.location.name}) from chee_index")
      while (rs.next) {
        val loc = rs.getString(1)
        buffer += resolveTo(root, loc)
      }
      buffer.toSeq
    }

  def locationInfo(): Try[LocationInfo] =
    withConn(dbfile) { implicit conn =>
      val buffer = collection.mutable.Map[File, Int]()
      val rs = sqlQuery("select location, count(*) from chee_index group by location")
      while (rs.next) {
        val loc = rs.getString(1)
        val count = rs.getInt(2)
        buffer.put(resolveTo(root, loc),  count)
      }
      LocationInfo(buffer.toMap)
    }


  final def insert[C](maps: Traversable[LazyMap], zero: C, p: Progress[Boolean, C]): Try[C] = {
    withConn(dbfile) { conn =>
      val (count, dur) = p.foreach(zero)(maps, insertProperties(root)(conn))
      logger.trace(s"Added $count properties in ${Timing.format(dur)}")
      count
    }
  }

  final def insertOne: MapGet[Try[Boolean]] = MapGet { lm =>
    val r = withConn(dbfile) { conn =>
      insertProperties(root)(conn).run(lm)
    }
    r match {
      case Success((next, b)) =>
        (next, Success(b))
      case Failure(ex) =>
        (lm, Failure(ex))
    }
  }

  final def update[C](maps: Traversable[LazyMap], param: UpdateParam = UpdateParam(), zero: C, p: Progress[Boolean, C]): Try[C] =
  {
    val uparam = param.copy(where = param.where.map(cond => root.map(relativeCond(_)(cond)) getOrElse cond))
    withConn(dbfile) { conn =>
      val (count, dur) = p.foreach(zero)(maps, updateProperties(uparam, root)(conn))
      logger.trace(s"Updated $count properties in ${Timing.format(dur)}")
      count
    }
  }

  final def updateOne(param: UpdateParam): MapGet[Try[Boolean]] = MapGet { lm =>
    val uparam = param.copy(where = param.where.map(cond => root.map(relativeCond(_)(cond)) getOrElse cond))
    val r = withConn(dbfile) { conn =>
      updateProperties(uparam, root)(conn).run(lm)
    }
    r match {
      case Success((next, b)) =>
        (next, Success(b))
      case Failure(ex) =>
        (lm, Failure(ex))
    }
  }

  final def move(source: File, target: File, newLocation: Option[File]): Try[Int] = {
    val src = root.map(paths.relative(_)(source)) getOrElse source.path
    val trg = root.map(paths.relative(_)(target)) getOrElse target.path
    val loc = root match {
      case Some(dir) => newLocation.map(paths.relative(dir))
      case None => newLocation.map(_.path)
    }
    val n = withConn(dbfile) { implicit conn =>
      sqlUpdate(SqlBackend.move(src, trg, source.isRegularFile, loc))
    }
    loc.map(p => LazyMap(Ident.location -> p.toString)).foreach { data =>
      val param = Index.UpdateParam(
        columns = MapGet.unit(Seq(Ident.location)),
        where = MapGet.valueForce(Ident.location).map(p => Prop(Comp.Like, Ident.location -> s"${p}*")))
      update(Seq(data), param, 0, Progress.empty[Boolean, Int])
    }
    n
  }

  final def delete(cond: Condition): Try[Int] = {
    val c = root.map(relativeCond(_)(cond)) getOrElse cond
    withConn(dbfile)(conn => deleteAll(c)(conn))
  }

  final def count(cond: Condition): Try[Int] = {
    val c = root.map(relativeCond(_)(cond)) getOrElse cond
    withConn(dbfile)(conn => SqliteBackend.count(c)(conn))
  }



  final def checksumMatch(path: String, checksum: String): Try[Boolean] = {
    val n = count(Condition.and(Prop(Comp.Eq, Ident.path -> path), Prop(Comp.Eq, Ident.checksum -> checksum)))
    n.map(_ == 1)
  }

  final def idExists: MapGet[Try[Boolean]] =
    MapGet.value(Ident.checksum).map {
      case Some(cs) => withConn(dbfile)(c => existsId(cs)(c))
      case None => Success(false)
    }

  final def pathExists(path: String): Try[Boolean] = {
    val p = root.map(paths.relativeStr(_)(path)) getOrElse path
    withConn(dbfile)(c => existsPath(p)(c))
  }

}

object SqliteBackend extends JdbcStatement with LazyLogging {

  logger.trace("Loading jdbc driver class")
  Class.forName("org.sqlite.JDBC")

  def existsId(checksum: String)(implicit conn: Connection): Boolean = {
    val rs = sqlQuery(s"select count(checksum) from chee_index where checksum = '$checksum'")
    if (rs.next) rs.getInt(1) > 0
    else false
  }

  def existsId(m: LazyMap)(implicit conn: Connection): (LazyMap, Boolean) = {
    val (map, Some(checksum)) = MapGet.value(Ident.checksum).run(m)
    (map, existsId(checksum))
  }

  def existsPath(path: String)(implicit conn: Connection): Boolean = {
    val rs = sqlQuery(s"select count(path) from chee_index where path = '$path'")
    if (rs.next) rs.getInt(1) > 0
    else false
  }

  def insertProperties(root: Option[File])(implicit conn: Connection): MapGet[Boolean] =
    relativeTo(root) {
      Index.realPath.flatMap { path =>
        if (existsPath(path.get)) MapGet.unit(false)
        else SqlBackend.insertStatement("chee_index").map(sql => { sqlUpdate(sql); true})
      }
    }

  def updateProperties(param: UpdateParam, root: Option[File])(implicit conn: Connection): MapGet[Boolean] = 
    relativeTo(root) {
      SqlBackend.updateRowStatement("chee_index", param.columns, param.where).map { sql =>
        sqlUpdate(sql) != 0
      }
    }

  def deleteAll(cond: Condition)(implicit conn: Connection): Int = {
    sqlUpdate(SqlBackend.deleteStatement("chee_index", cond))
  }

  def count(cond: Condition)(implicit conn: Connection): Int = {
    val rs = sqlQuery(SqlBackend.count("chee_index", cond))
    rs.next
    rs.getInt(1)
  }

  def resolveTo(root: Option[File], name: String) =
    root match {
      case Some(r) =>
        File(paths.resolve(r)(name))
      case None =>
        File(name)
    }

  def relativeTo[T](root: Option[File])(m: MapGet[T]): MapGet[T] = root match {
    case Some(dir) => paths.relativeTo(dir).flatMap(_ => m)
    case _ => m
  }

  def resolveTo(root: Option[File])(maps: Stream[LazyMap]): Stream[LazyMap] = root match {
    case Some(dir) => MapGet.filter(maps, paths.resolveTo(dir).map(_ => true))
    case _ => maps
  }

  def relativeCond(root: File)(c: Condition): Condition = Condition.mapAll({
    case Prop(comp, Property(id, name))
        if id.is(Ident.location) || id.is(Ident.path) =>
      Prop(comp, Property(id, paths.relativeStr(root)(name)))
    case In(id, values)
        if id.is(Ident.location) || id.is(Ident.path) =>
      In(id, values.map(paths.relativeStr(root)))
    case n => n
  })(c)
}
