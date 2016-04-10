package chee.cli

import better.files.File.LinkOptions
import com.typesafe.config.Config
import better.files._
import chee.properties._
import chee.query._
import chee.CheeConf.Implicits._

abstract class AbstractLs extends ScoptCommand {
  import AbstractLs._

  trait CommandOpts {
    def lsOpts: LsOpts
  }

  type T <: CommandOpts

  case class LsOpts(
    directory: Option[File] = None,
    recursive: Boolean = false,
    all: Boolean = false,
    first: Option[Int] = None,
    indexed: Option[Boolean] = None,
    query: String = "")

  abstract class LsOptionParser extends CheeOptionParser[T](name) {
    opt[File]('f', "file") optional() action { (f, c) =>
      copyLsOpts(c, c.lsOpts.copy(directory = Some(f)))
    } textW ("A directory to search instead of the index. It can also be a "+
      "file, in which case a query and the `-r' flag are ignored.")

    opt[Unit]('r', "recursive") optional() action { (_, c) =>
      copyLsOpts(c, c.lsOpts.copy(recursive = true))
    } textW ("Find files recursively. Only applicable if `-f' is specified.")

    opt[Boolean]('i', "indexed") action { (b, c) =>
      copyLsOpts(c, c.lsOpts.copy(indexed = Some(b)))
    } textW ("Find indexed or not indexed files. Only applicable if `-f' is specified.")

    opt[Unit]('a', "all") optional() action { (_, c) =>
      copyLsOpts(c, c.lsOpts.copy(all = true))
    } textW ("When used with `-f', ignore the default query, otherwise select non-existing files.")

    opt[Int]("first") valueName("<n>") optional() action { (n, c) =>
      copyLsOpts(c, c.lsOpts.copy(first = Some(n)))
    } text ("Limit output to the first n items.")

    moreOptions()

    arg[String]("<query>") optional() unbounded() action { (q, c) =>
      copyLsOpts(c, c.lsOpts.copy(query = c.lsOpts.query +" "+ q))
    } textW ("The query string. See the manual page about queries for more information.")

    def moreOptions(): Unit

    def copyLsOpts(o: T, lsopts: LsOpts): T
  }

  def exec(cfg: Config, t: T): Unit = {
    val opts = t.lsOpts
    val props = getQueryCondition(cfg, opts) match {
      case Right(cond) =>
        opts.directory match {
          case Some(dir) =>
            val r = dir match {
              case Directory(d) =>
                FileBackend.find(cond, d, opts.recursive)
              case RegularFile(f) =>
                Stream(LazyMap.fromFile(f).add(Ident.location -> f.parent.pathAsString))
              case _ =>
                Stream.empty[LazyMap]
            }
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
    exec(cfg, t, (opts.first match {
      case Some(n) => props.take(n)
      case _ => props
    }))
  }

  def exec(cfg: Config, opts: T, props: Stream[LazyMap]): Unit

  def getQueryCondition(cfg: Config, opts: LsOpts): Either[String, Condition] = {
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
