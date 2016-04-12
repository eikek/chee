package chee.cli

import chee.CheeConf.Implicits._
import chee.cli.LsOptions.{Opts => LsOpts}
import chee.properties.Patterns._
import com.typesafe.config.Config

object Find extends ScoptCommand with AbstractLs {

  type T = Opts

  val name = "find"
  val defaults = Opts()

  case class Opts(
    lsOpts: LsOptions.Opts = LsOptions.Opts(),
    pattern: Option[String] = None) {
    def updateLsOpts(f: LsOpts =>  LsOpts) =
      copy(lsOpts = f(lsOpts))
  }

  val parser = new Parser with LsOptions[Opts] {
    addLsOptions(_ updateLsOpts _, title = None)

    opt[String]('p', "pattern") optional() action { (p, c) =>
      c.copy(pattern = Some(p))
    } text ("The format pattern.")

    addQuery(_ updateLsOpts _)
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
