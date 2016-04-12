package chee.cli

import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}

object ConfigCmd extends ScoptCommand {

  case class Opts(
    render: ConfigRenderOptions = ConfigRenderOptions.defaults().setOriginComments(false).setJson(false),
    all: Boolean = false
  )

  type T = Opts

  val defaults = Opts()

  val name = "config"

  val parser = new Parser {
    opt[Unit]("origin") action { (_, c) =>
      c.copy(render = c.render.setOriginComments(true))
    } text ("Print comments showing the origin of the value.")

    opt[Unit]("json") action { (_, c) =>
      c.copy(render = c.render.setJson(true).setComments(false))
    } text ("Render the configuration in JSON.")

    opt[Unit]("all") action { (_, c) =>
      c.copy(all = true)
    } text ("Show complete config, with system properties.")
  }

  def exec(cfg: Config, opts: Opts): Unit = {
    if (opts.all) outln(cfg.root().render(opts.render))
    else {
      val c = ConfigFactory.empty().withValue("chee", cfg.getValue("chee"))
      outln(c.root().render(opts.render))
    }
  }
}
