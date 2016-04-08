package chee.cli

import better.files._
import chee.query.Progress
import com.typesafe.config.Config
import chee.CheeConf.Implicits._
import chee.Size
import chee.properties._
import chee.properties.Patterns._
import chee.Processing
import org.bouncycastle.openpgp.PGPPublicKey

object Encrypt extends AbstractLs {

  val name = "encrypt"

  case class Opts(
    lsOpts: LsOpts = LsOpts(),
    parallel: Boolean = false,
    pattern: Option[String] = None,
    symmetric: Boolean = false,
    passphrase: Option[Array[Char]] = None,
    keyFile: Option[File] = None,
    keyId: Option[String] = None,
    pubKey: Option[PGPPublicKey] = None
  ) extends CommandOpts

  type T = Opts

  val defaults = Opts()

  val parser = new LsOptionParser {
    def copyLsOpts(o: Opts, lso: LsOpts) = o.copy(lsOpts = lso)

    opt[Unit]('c', "concurrent") action { (_, c) =>
      c.copy(parallel = true)
    } text("Process files concurrently.")

    opt[String]('p', "pattern") action { (p, c) =>
      c.copy(pattern = Some(p))
    } text ("The format pattern used to print the result to stdout.")

    def moreOptions(): Unit = ()
  }

  val progress = Progress.seq[Unit, Int](
    Progress.after { n => n + 1},
    Progress.done { (n, dur) =>
      logger.trace(s"Encrypted $n files in ${chee.Timing.format(dur)}")
    }
  )

  def processingAction(cfg: Config, opts: Opts): MapGet[Boolean] = ???

  // 1. encrypt file to tmpdir
  // 2. move the encrypted file next to original
  // 3. update index with new path
  // 4. delete original

  def exec(cfg: Config, opts: Opts, props: Stream[LazyMap]): Unit = {
    Scale.getFormat(cfg, opts.pattern) match {
      case Right(pattern) =>
        val proc = processingAction(cfg, opts)
        val action = MapGet.get.map(m => out(pattern.right(userError).result(m)))
        if (opts.parallel) {
          progress.foreach(0)(MapGet.parfilter(props, proc), action)
        } else {
          progress.foreach(0)(MapGet.filter(props, proc), action)
        }
      case Left(msg) =>
        userError(msg)
    }
  }
}
