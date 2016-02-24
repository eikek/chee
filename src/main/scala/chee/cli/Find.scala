package chee.cli

import com.typesafe.config.Config
import chee.properties._
import chee.properties.Patterns._
import chee.CheeConf.Implicits._

object Find extends AbstractLs {

  type T = Opts

  val name = "find"
  val defaults = Opts()

  case class Opts(
    lsOpts: LsOpts = LsOpts(),
    pattern: Option[String] = None) extends CommandOpts

  val parser = new LsOptionParser {
    def copyLsOpts(o: Opts, lso: LsOpts) = o.copy(lsOpts = lso)
    def moreOptions(): Unit = {
      opt[String]('p', "pattern") optional() action { (p, c) =>
        c.copy(pattern = Some(p))
      } text ("The format pattern.")
    }
  }

  def exec(cfg: Config, opts: T, props: Stream[LazyMap]): Unit = {
    cfg.getFormat(opts.pattern, "chee.formats.default-find-format") match {
      case Right(pattern) => props.foreach { m =>
        out(pattern.right(userError).result(m))
      }
      case Left(msg) => chee.UserError(msg)
    }
  }
}
