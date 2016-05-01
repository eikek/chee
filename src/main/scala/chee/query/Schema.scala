package chee.query

import chee.properties.Ident
import com.typesafe.scalalogging.LazyLogging
import java.sql.Connection
import scala.util.Try


trait Schema {
  def create(implicit conn: Connection): Unit
}

object Schema extends JdbcStatement with LazyLogging {
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
