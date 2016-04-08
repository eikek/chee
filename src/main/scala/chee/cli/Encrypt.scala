package chee.cli

import better.files._
import chee.crypto.KeyFind
import chee.query.{ Progress, SqliteBackend }
import com.typesafe.config.Config
import chee.CheeConf.Implicits._
import chee.CheeConf.CryptMethod
import chee.Size
import chee.properties._
import chee.properties.MapGet._
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
    keyId: Option[String] = None
  ) extends CommandOpts

  type T = Opts

  val defaults = Opts()

  val parser = new LsOptionParser {
    def copyLsOpts(o: Opts, lso: LsOpts) = o.copy(lsOpts = lso)

    def moreOptions(): Unit = {
      opt[Unit]('c', "concurrent") action { (_, c) =>
        c.copy(parallel = true)
      } text("Process files concurrently.")

      opt[String]('p', "pattern") action { (p, c) =>
        c.copy(pattern = Some(p))
      } text ("The format pattern used to print the result to stdout.")

      opt[String]("keyid") action { (k, c) =>
        c.copy(keyId = Some(k))
      }

      opt[File]("key-file") action { (f, c) =>
        c.copy(keyFile = Some(f))
      }
    }
  }

  val progress = Progress.seq[Unit, Int](
    Progress.after { n => n + 1},
    Progress.done { (n, dur) =>
      logger.trace(s"Encrypted $n files in ${chee.Timing.format(dur)}")
    }
  )

  def findPublicKey(cfg: Config, opts: Opts): PGPPublicKey = {
    val keyFile = opts.keyFile.getOrElse(cfg.getFile("chee.crypt.public-key-file"))
    val keyId = opts.keyId.getOrElse(cfg.getString("chee.crypt.key-id"))
    if (!keyFile.exists) userError(s"Key file `${keyFile}' does not exists!")
    if (keyId.isEmpty) userError("No keyId specified!")
    KeyFind.findPublicKey(keyFile, keyId)
  }

  def findPassphrase(cfg: Config, opts: Opts): Array[Char] = {
    val p = opts.passphrase.getOrElse(cfg.getString("chee.crypt.default-password").toCharArray)
    if (p.isEmpty) userError("No passphrase given!")
    p
  }

  def postProcess(sqlite: SqliteBackend): MapGet[Boolean] =
    pair(path, valueForce(Processing.encryptedPath).map(File(_))).flatMap {
      case (orig, enc) if enc.exists =>
        modify(m => sqlite.updateOne(
          m.remove(Processing.encryptedPath)
            .add(Ident.path -> enc.pathAsString), Ident.checksum).get._1)
          .map(_ => {
            if (orig.exists) orig.delete()
            true
          })
      case _ =>
        unit(false)
    }

  def processingAction(cfg: Config, opts: Opts): MapGet[Boolean] = {
    val sqlite = new SqliteBackend(cfg.getIndexDb)
    cfg.getCryptMethod match {
      case CryptMethod.Password =>
        val pass = findPassphrase(cfg, opts)
        val out = path.map(_.mapFileName(_ + ".pbc"))
        ???
      case CryptMethod.Pubkey =>
        val key = findPublicKey(cfg, opts)
        val out = path.map(_.mapFileName(_ + ".pkc"))
        Processing.encryptPubkey(key, out).flatMap {
          case true => postProcess(sqlite)
          case false => unit(false)
        }
    }
  // 0. get the key/passphrase
  // 1. encrypt file to origin+.pkc|pbc
  // 2. update index with new path
  // 3. delete original
  }

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
