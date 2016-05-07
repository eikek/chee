package chee.cli

import MetaFind._
import LsOptions.{ Opts => LsOpts }
import com.typesafe.config.Config
import chee.conf._
import chee.properties.Patterns._

class MetaFind extends ScoptCommand with LockSupport {

  val name = "find"
  type T = Opts
  val defaults = Opts()

  val parser = new Parser with LsOptions[Opts] {
    skip  { (c, f) => c.copy(skip = f(LsOpts(skip = c.skip)).skip) }
    first  { (c, f) => c.copy(first = f(LsOpts(first = c.first)).first) }

    opt[String]('p', "pattern") optional() action { (p, c) =>
      c.copy(pattern = Some(p))
    } text ("The format pattern.")

    queryArg { (c, f) => c.copy(query = f(LsOpts(query = c.query)).query) }

    checkConfig { cfg =>
      if (cfg.query.nonEmpty) success
      else failure(wrapLines(70)("No query specified. If you really want to select all records, use a query like `checksum?'."))
    }
  }

  def exec(cfg: Config, opts: Opts): Unit = withLock(cfg) {
    cfg.getFormat(opts.pattern, "chee.formats.default-metafind-format") match {
      case Right(pattern) => cfg.getMetadataFile.query(opts.query) match {
        case Right(props) =>
          sliced(opts.first, opts.skip)(props).foreach { m =>
            out(pattern.right(userError).result(m))
          }
        case Left(msg) => userError(msg)
      }
      case Left(msg) => userError(msg)
    }
  }
}

object MetaFind {

  case class Opts(
    query: String = "",
    skip: Option[Int] = None,
    first: Option[Int] = None,
    pattern: Option[String] = None
  )
}
