package chee.query

import java.sql.{Connection, ResultSet}

import better.files._
import chee.Timing
import chee.properties.{Ident, LazyMap, Property}
import chee.util.paths
import com.typesafe.scalalogging.{LazyLogging, Logger}


trait JdbcStatement {

  protected val logger: Logger

  final def sqlQuery(sql: String)(implicit conn: Connection): ResultSet = {
    logger.debug(s"Running sql query: $sql")
    Timing.timed((_: ResultSet, d) => logger.trace(s"sql query ran in ${Timing.format(d)}")) {
      conn.createStatement.executeQuery(sql)
    }
  }

  final def sqlUpdate(sql: String)(implicit conn: Connection): Int = {
    logger.debug(s"Running sql update: $sql")
    Timing.timed((_: Int, d) => logger.trace(s"sql update ran in ${Timing.format(d)}")) {
      conn.createStatement.executeUpdate(sql)
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
    def toPropertyMap(root: Option[File]): LazyMap = {
      val path = (root, rs.getString(Ident.path.name)) match {
        case (Some(dir), name) =>
          paths.resolve(dir)(name)
        case (None, name) =>
          name
      }
      val map = LazyMap.fromFile(File(path))
      SqlBackend.idents.foldLeft(map) { (m, id) =>
        val value = Option(rs.getObject(id.name)).map(_.toString)
        m.add(value.map(v => Property(id, v)))
      }
    }
  }
}
