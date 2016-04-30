package chee.cli

import com.typesafe.config.Config
import chee.properties._
import chee.query._
import chee.conf._
import chee.util.files._
import chee.cli.LsOptions.{ Opts => LsOpts }
import AbstractLs._

trait AbstractLs {

  private def isIndexed(cfg: Config)(flag: Boolean): MapGet[Boolean] = {
    val sqlite = new SqliteBackend(cfg.getIndexDb)
    sqlite.idExists.map(_.get == flag)
  }

  private def directoryFind(cond: Condition, cfg: Config, opts: LsOpts): Option[Stream[LazyMap]] = {
    val props = opts.directory map {
      case Directory(d) =>
        FileBackend.find(cond, d, opts.recursive)
      case RegularFile(f) =>
        Stream(LazyMap.fromFile(f).add(Ident.location -> f.parent.pathAsString))
      case _ =>
        Stream.empty[LazyMap]
    }
    props.map { p =>
      opts.indexed.map(isIndexed(cfg))
        .map(MapGet.filter(p, _))
        .getOrElse(p)
    }
  }

  private def indexFind(cond: Condition, cfg: Config, opts: LsOpts): Stream[LazyMap] = {
    val sqlite = new SqliteBackend(cfg.getIndexDb)
    val filter = if (opts.all) MapGet.unit(true) else Predicates.fileExists
    MapGet.filter(sqlite.find(cond).get, filter)
  }

  private def lift(f: (Stream[LazyMap], Int) => Stream[LazyMap]): Option[Int] => Stream[LazyMap] => Stream[LazyMap] =
    n => s => n.map(f(s, _)).getOrElse(s)

  def find(cfg: Config, opts: LsOpts): Stream[LazyMap] =
    getQueryCondition(cfg, opts) match {
      case Right(cond) =>
        val stream = directoryFind(cond, cfg, opts) getOrElse indexFind(cond, cfg, opts)
        val x: Int => Stream[LazyMap] = stream.drop _
        val optSlice = lift(_ drop _)(opts.skip) andThen lift(_ take _)(opts.first)
        optSlice(stream)
      case Left(msg) =>
        userError(msg)
    }

  def getQueryCondition(cfg: Config, opts: LsOpts): Either[String, Condition] =
    opts.directory match {
      case Some(_) => getFileCondition(cfg.makeQuery, opts.query, cfg, opts.all)
      case _ => getIndexCondition(cfg.makeQuery, opts.query)
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
