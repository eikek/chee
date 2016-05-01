package chee.query

import better.files._
import chee.Timing
import com.typesafe.scalalogging.LazyLogging
import java.sql.{ Connection, DriverManager, SQLException }
import scala.util.{ Failure, Success, Try }


trait JdbcConnection extends JdbcStatement {
  self: LazyLogging =>

  val jdbcUrl: String

  final def withConn[A](dbfile: File)(body: Connection => A): Try[A] = {
    Timing.timed((_: Try[A], d) => logger.trace(s"JDBC Code Block took ${Timing.format(d)}")) {
      logger.trace("[[[ open sqlite connecion")
      val conn = DriverManager.getConnection(s"jdbc:sqlite:${dbfile.path}")
      val result = Try(body(conn)).recoverWith {
        case e: SQLException =>
          Try(initialize(conn)) match {
            case Success(_) =>
              Try(body(conn))
            case Failure(e2) =>
              e.addSuppressed(e2)
              Failure(e)
          }
      }
      conn.close
      logger.trace("closed sqlite connection ]]]")
      result
    }
  }

}
