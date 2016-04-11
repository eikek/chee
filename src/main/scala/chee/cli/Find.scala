package chee.cli

import com.typesafe.config.Config
import chee.properties.Patterns._
import chee.CheeConf.Implicits._

object Find extends ScoptCommand with AbstractLs {

  type T = Opts

  val name = "find"
  val defaults = Opts()

  case class Opts(
    lsOpts: LsOptions.Opts = LsOptions.Opts(),
    pattern: Option[String] = None)

  val parser = new Parser with LsOptions[Opts] {
    addLsOptions((c, f) => c.copy(lsOpts = f(c.lsOpts)))

    opt[String]('p', "pattern") optional() action { (p, c) =>
      c.copy(pattern = Some(p))
    } text ("The format pattern.")

    queryArg((c, f) => c.copy(lsOpts = f(c.lsOpts)))
  }

  def exec(cfg: Config, opts: Opts): Unit = {
    val props = find(cfg, opts.lsOpts)
    cfg.getFormat(opts.pattern, "chee.formats.default-find-format") match {
      case Right(pattern) => props.foreach { m =>
        out(pattern.right(userError).result(m))
      }
      case Left(msg) => chee.UserError(msg)
    }
  }
}
