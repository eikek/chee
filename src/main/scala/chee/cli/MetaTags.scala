package chee.cli

import MetaFind._
import com.typesafe.config.Config
import chee.conf._
import chee.properties.Patterns._

class MetaTags extends ScoptCommand with LockSupport {

  val name = "tags"
  type T = Opts
  val defaults = Opts()

  val parser = new Parser with LsOptions[Opts] {
    opt[String]('p', "pattern") optional() action { (p, c) =>
      c.copy(pattern = Some(p))
    } text ("The format pattern.")
  }

  def exec(cfg: Config, opts: Opts): Unit = withLock(cfg) {
    cfg.getFormat(opts.pattern, "chee.formats.default-tags-format") match {
      case Right(pattern) =>
        val tagCloud = cfg.getMetadataFile.listTags
        tagCloud.toMaps.foreach { m =>
          out(pattern.right(userError).result(m))
        }
      case Left(msg) => chee.UserError(msg)
    }
  }
}

object MetaTags {

  case class Opts(
    pattern: Option[String] = None
  )
}
