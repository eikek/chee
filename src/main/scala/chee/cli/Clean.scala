package chee.cli

import chee.CheeConf.Implicits._
import com.typesafe.config.Config

object Clean extends ScoptCommand {

  case class Opts(
    tempDir: Option[String] = None,
    decryptDir: Option[String] = None,
    scaleDir: Option[String] = None
  )

  type T = Opts

  val defaults = Opts()

  val name = "clean"

  val parser = new Parser {
    opt[Unit]('t', "temp") action { (_, c) =>
      c.copy(tempDir = Some("chee.tmpdir"))
    } textW ("Remove chee's directory storing other temporary data.")

    opt[Unit]('d', "decrypt") action { (_, c) =>
      c.copy(decryptDir = Some("chee.crypt.decrypt-temp"))
    } textW ("Remove the directory containing temporary decrypted data.")

    opt[Unit]('s', "scaled") action { (_, c) =>
      c.copy(scaleDir = Some("chee.scaleddir"))
    } textW ("Remove the directory containing scaled images and thumbnails.")

    opt[Unit]("all") action { (_, c) =>
      Opts(Some("chee.tmpdir"), Some("chee.scaleddir"), Some("chee.crypt.decrypt-temp"))
    } textW ("Remove all temporary data.")

    checkConfig { c =>
      if (c.tempDir.orElse(c.decryptDir).orElse(c.scaleDir).nonEmpty) success
      else failure("One of the flags must be set.")
    }
  }

  def removeDir(cfg: Config, key: String): Unit = cfg.getFile(key) match {
    case Directory(dir) => dir.delete()
    case _ =>
  }

  def exec(cfg: Config, opts: Opts): Unit = {
    if (opts.tempDir.nonEmpty) {
      outln(s"Deleting ${cfg.getString(opts.tempDir.get)}")
      removeDir(cfg, opts.tempDir.get)
    }

    if (opts.scaleDir.nonEmpty) {
      outln(s"Deleting ${cfg.getString(opts.scaleDir.get)}")
      removeDir(cfg, opts.scaleDir.get)
    }

    if (opts.decryptDir.nonEmpty) {
      outln(s"Deleting ${cfg.getString(opts.decryptDir.get)}")
      removeDir(cfg, opts.decryptDir.get)
    }
  }
}
