package chee

import java.util.concurrent.atomic.AtomicReference

import scala.sys.process.{Process, ProcessLogger}
import scala.util.Try

import better.files._
import chee.crypto._
import chee.properties._
import chee.properties.Patterns._
import chee.query._
import chee.util.files._
import com.sksamuel.scrimage.ScaleMethod
import com.typesafe.config.Config

package object conf {

  implicit class ConfigOps(cfg: Config) {

    def getFile(key: String): File = File(cfg.getString(key))

    def getCommand(key: String): List[String] =
      cfg.getString(key).split("\\s+").filter(_.nonEmpty).toList

    def getIndexDb = getFile("chee.dbfile")

    def getSystemConfig = ConfigFile(getFile("chee.system-config"))
    def getLocationConf = new LocationConf(getSystemConfig, getRepoRoot)
    def getCollectionConf = new CollectionConf(getSystemConfig)

    def makeQuery: Query = Query.create(
      QuerySettings(LocalDateTime.now, getCollectionConf.list.get))

    def getFormat(p: Option[String], fallbackKey: String): Either[String, Pattern] = {
      def forPattern(pattern: String) = pattern match {
        case "" => Right(FormatPatterns.oneline)
        case "oneline" => Right(FormatPatterns.oneline)
        case "lisp" => Right(FormatPatterns.lisp)
        case "detail" => Right(FormatPatterns.detail)
        case "paths" => Right(FormatPatterns.paths)
        case _ => findCustomFormat(pattern)
      }
      p.map(forPattern).getOrElse {
        forPattern(cfg.getString(fallbackKey))
      }
    }

    def findCustomFormat(name: String): Either[String, Pattern] = {
      val formats = cfg.getConfig("chee.formats")
      if (Try(formats.hasPath(name)).getOrElse(false)) FormatPatterns.parse(formats.getString(name))
      else FormatPatterns.parse(name)
    }

    def fileDefaultQuery(query: Query): Either[String, Condition] = {
      val key = "chee.queries.file-default"
      val defq = cfg.getString(key)
      query(defq) match {
        case Left(msg) => Left("Default query has errors. \n" +
            s"Please check the config file at key `${key}'.\n" + msg)
        case x => x
      }
    }

    def getScaleMethod(key: String): ScaleMethod = {
      cfg.getString(key).toLowerCase() match {
        case "fastscale" => ScaleMethod.FastScale
        case "bicubic" => ScaleMethod.Bicubic
        case "lanczos3" => ScaleMethod.Lanczos3
        case "bspline" => ScaleMethod.BSpline
        case "bilinear" => ScaleMethod.Bilinear
        case v => UserError(s"No scale method: `$v'. Check your config at key `$key'.")
      }
    }

    def getCryptMethod: CryptMethod = {
      cfg.getString("chee.crypt.default-encryption").toLowerCase() match {
        case "pubkey" => CryptMethod.Pubkey
        case "password" => CryptMethod.Password
        case v => UserError(s"Invalid config value for `chee.crypt.default.encryption': `${v}'")
      }
    }

    def getAlgorithm: Algorithm = {
      val v = cfg.getString("chee.crypt.algorithm")
      Algorithm.find(v) match {
        case Some(a) => a
        case None => UserError(s"Invalid config value for `chee.crypt.algorithm': `${v}")
      }
    }

    def readPasswordFromFile(key: String) =
      getFile(key).readPassword

    def readPasswordFromCommand(key: String): Option[Array[Char]] = {
      cfg.getCommand(key) match {
        case Nil => None
        case cmd =>
          val pw = new AtomicReference[String]()
          Process(cmd.head, cmd.tail) !< ProcessLogger(pw.compareAndSet(null, _), Console.err.println) match {
            case 0 => Option(pw.get).map(_.toCharArray)
            case rc => UserError(s"Password command returned non-zero: $rc")
          }
      }
    }

    def getRepoRoot: Option[File] =
      Option(cfg.getString("chee.repo.root"))
        .filter(_.nonEmpty)
        .map(File(_))
  }
}
