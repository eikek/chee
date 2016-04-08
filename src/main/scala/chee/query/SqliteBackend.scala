package chee.query

import java.sql._
import chee.properties._
import better.files._
import scala.util.{Try, Success, Failure}
import com.typesafe.scalalogging.LazyLogging
import chee.Timing
import java.time.Duration
import java.nio.file.Path

class SqliteBackend(dbfile: File, pageSize: Int = 500) extends LazyLogging {
  import SqliteBackend._

  final def find(cond: Condition): Try[Stream[LazyMap]] = {
    val sql = (s"""SELECT ${SqlBackend.idents.map(_.name).mkString(",")},coalesce(created,lastmodified) as sorting"""
      + s""" FROM chee_index WHERE ${SqlBackend.whereClause(cond)}"""
      + s""" ORDER BY sorting""")
    findNext(sql, 0).recoverWith {
      case ex => Failure(new RuntimeException(s"Error for sql: $sql", ex))
    }
  }

  final def findNext(sql: String, skip: Int): Try[Stream[LazyMap]] = {
    val first = withConn(dbfile) { implicit conn =>
      val buffer = collection.mutable.ListBuffer[LazyMap]()
      val rs = sqlQuery(sql + s" limit $skip,$pageSize")
      while (rs.next) {
        buffer += rs.toPropertyMap
      }
      buffer.toStream
    }
    Try {
      val f = first.get
      if (f.isEmpty) f
      else f #::: findNext(sql, skip+pageSize).get
    }
  }

  final def insert[C](maps: Iterable[LazyMap], zero: C, p: Progress[Boolean, C]): Try[Unit] = {
    withConn(dbfile) { conn =>
      val (count, dur) = p.foreach(zero)(maps, insertProperties(conn))
      logger.trace(s"Added $count properties in ${Timing.format(dur)}")
    }
  }

  final def insertOne(lm: LazyMap): Try[(LazyMap, Boolean)] =
    withConn(dbfile) { conn =>
      insertProperties(conn).run(lm)
    }

  final def checksumMatch(path: String, checksum: String): Try[Boolean] = {
    val n = count(Condition.and(Prop(Comp.Eq, Ident.path -> path), Prop(Comp.Eq, Ident.checksum -> checksum)))
    n.map(_ == 1)
  }

  final def update(maps: Iterable[LazyMap], where: Ident = Ident.path, p: Progress[Boolean, Int] = Progress.empty): Try[Unit] = {
    withConn(dbfile) { conn =>
      val (count, dur) = p.foreach(0)(maps, updateProperties(where)(conn))
      logger.trace(s"Updated $count properties in ${Timing.format(dur)}")
    }
  }

  final def updateOne(lm: LazyMap, where: Ident = Ident.path): Try[(LazyMap, Boolean)] =
    withConn(dbfile) { conn =>
      updateProperties(where)(conn).run(lm)
    }

  final def changeLocation(old: Path, next: Path): Try[Int] =
    withConn(dbfile) { implicit conn =>
      val len = old.toString.length + 1
      sqlUpdate(s"UPDATE chee_index SET location = '$next', path = '$next' || substr(path, $len) WHERE location = '$old'")
    }

  final def delete(cond: Condition): Try[Int] =
    withConn(dbfile)(c => deleteAll(cond)(c))    

  final def count(cond: Condition): Try[Int] =
    withConn(dbfile)(c => SqliteBackend.count(cond)(c))

  final def idExists: MapGet[Try[Boolean]] =
    MapGet.value(Ident.checksum).map {
      case Some(cs) => withConn(dbfile)(c => existsId(cs)(c))
      case None => Success(false)
    }

  final def pathExists(path: String): Try[Boolean] =
    withConn(dbfile)(c => existsPath(path)(c))

  final def createSchema: Try[Unit] = withConn(dbfile)(initialize)

}

object SqliteBackend extends LazyLogging {
  import scala.language.implicitConversions
  logger.trace("Loading jdbc driver class")
  Class.forName("org.sqlite.JDBC")

  def withConn[A](dbfile: File)(body: Connection => A): Try[A] = Try {
    Timing.timed((_: A, d) => logger.trace(s"JDBC Code Block took ${Timing.format(d)}")) {
      val conn = DriverManager.getConnection(s"jdbc:sqlite:${dbfile.path}")
      try {
        logger.trace("[[[ open sqlite connecion")
        try {
          body(conn)
        } catch {
          case e1: SQLException =>
            try { initialize(conn) } catch {
              case e2: Exception =>
                e1.addSuppressed(e2)
                throw e1
            }
            body(conn)
        }
      } finally {
        conn.close
        logger.trace("closed sqlite connection ]]]")
      }
    }
  }

  def sqlQuery(sql: String)(implicit conn: Connection): ResultSet = {
    logger.debug(s"Running sql query: $sql")
    Timing.timed((_: ResultSet, d) => logger.trace(s"sql query ran in ${Timing.format(d)}")) {
      conn.createStatement.executeQuery(sql)
    }
  }

  def sqlUpdate(sql: String)(implicit conn: Connection): Int = {
    logger.debug(s"Running sql update: $sql")
    Timing.timed((_: Int, d) => logger.trace(s"sql update ran in ${Timing.format(d)}")) {
      conn.createStatement.executeUpdate(sql)
    }
  }

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

  def insertProperties(implicit conn: Connection): MapGet[Boolean] = 
    MapGet.value(Ident.path).flatMap { path => 
      if (existsPath(path.get)) MapGet.unit(false)
      else SqlBackend.insertStatement("chee_index").map(sql => { sqlUpdate(sql); true})
    }

  def updateProperties(where: Ident)(implicit conn: Connection): MapGet[Boolean] = {
    SqlBackend.updateRowStatement("chee_index", where).map { sql =>
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

  trait Schema {
    def create(implicit conn: Connection): Unit
  }
    
  object Schema {
    def getVersion(implicit conn: Connection): Int = {
      val v = Try {
        val rs = sqlQuery("select max(version) from chee_schema_version")
        if (rs.next) rs.getInt(1)
        else sys.error(s"Unknown database state. The version table exists, but is empty.")
      }
      v.getOrElse(-1)
    }
    val migrations: Map[Int, Schema] = Map(
      1 -> InitialSchema
    )
  }

  object InitialSchema extends Schema with LazyLogging {
    def create(implicit conn: Connection): Unit = {
      logger.info("Running initial schema migration, to version 1")
      val stmt = conn.createStatement
      stmt.executeUpdate("CREATE TABLE chee_schema_version (version integer)")
      stmt.executeUpdate(SqlBackend.createTable("chee_index"))
      stmt.executeUpdate("CREATE UNIQUE INDEX path_idx ON chee_index (path)")
      for (id <- (SqlBackend.idents.filter(_ != Ident.path))) {
        stmt.executeUpdate(s"CREATE INDEX ${id.name}_idx ON chee_index (${id.name})")
      }
      stmt.executeUpdate("INSERT INTO chee_schema_version VALUES (1)")
      ()
    }
  }

  def initialize(conn: Connection): Unit = {
    val version = Schema.getVersion(conn)
    val migs = Schema.migrations.keys.dropWhile(_ <= version).toList.sorted
    for (mig <- migs) {
      Schema.migrations(mig).create(conn)
    }
  }

  implicit class ResultSetOps(rs: ResultSet) {
    import better.files._
    def toPropertyMap: LazyMap = {
      val path = rs.getString(Ident.path.name)
      val map = LazyMap.fromFile(File(path))
      SqlBackend.idents.foldLeft(map) { (m, id) =>
        val value = Option(rs.getObject(id.name)).map(_.toString)
        m.add(value.map(v => Property(id, v)))
      }
    }
  }

}
