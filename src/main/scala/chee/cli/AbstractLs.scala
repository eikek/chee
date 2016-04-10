package chee.cli

import com.typesafe.config.Config
import chee.properties._
import chee.query._
import chee.CheeConf.Implicits._

trait AbstractLs { self: ScoptCommand =>
  import AbstractLs._

  def find(cfg: Config, opts: LsOptions.Opts): Stream[LazyMap] = {
    val props = getQueryCondition(cfg, opts) match {
      case Right(cond) =>
        opts.directory match {
          case Some(dir) =>
            val r = FileBackend.find(cond, dir, opts.recursive)
            opts.indexed match {
              case Some(b) =>
                val sqlite = new SqliteBackend(cfg.getIndexDb)
                MapGet.filter(r, sqlite.idExists.map(_.get == b))
              case _ =>
                r
            }
          case _ =>
            val sqlite = new SqliteBackend(cfg.getIndexDb)
            val r = sqlite.find(cond).get
            val filter = if (opts.all) MapGet.unit(true) else Predicates.fileExists
            MapGet.filter(r, filter)
        }
      case Left(msg) =>
        chee.UserError(msg)
    }
    opts.first match {
      case Some(n) => props.take(n)
      case _ => props
    }
  }

  def getQueryCondition(cfg: Config, opts: LsOptions.Opts): Either[String, Condition] = {
    val query = cfg.makeQuery
    opts.directory match {
      case Some(_) => getFileCondition(query, opts.query, cfg, opts.all)
      case _ => getIndexCondition(query, opts.query)
    }
  }
}

object AbstractLs {
  def getIndexCondition(q: Query, query: String): Either[String, Condition] = {
    if (query.trim.isEmpty) Right(TrueCondition)
    else q(query.trim)
  }

  def getFileCondition(q: Query, query: String, cfg: Config, all: Boolean): Either[String, Condition] = {
    val defquery =
      if (all) Right(TrueCondition)
      else cfg.fileDefaultQuery(q)

    if (query.trim.isEmpty) defquery
    else for {
      q1 <- defquery.right
      q2 <- q(query.trim).right
    } yield Condition.and(q1, q2)
  }
}
