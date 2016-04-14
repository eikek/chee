package chee

import chee.cli.RegularFile
import chee.crypto.{ Algorithm, CryptMethod }
import com.typesafe.config.{Config, ConfigFactory}
import java.util.concurrent.atomic.AtomicReference
import scala.sys.process.{ Process, ProcessLogger }
import scala.util.Try
import better.files._
import chee.properties._
import chee.query._
import Patterns._
import com.typesafe.scalalogging.LazyLogging
import com.sksamuel.scrimage.ScaleMethod

object CheeConf {
  // for scala.concurrent.ExecutionContext.Implicits.global
  System.setProperty("scala.concurrent.context.numThreads", "x1.5")

  private val debug = {
    System.getProperty("chee.debugConfig", "false") == "true"
  }

  def readPasswordFromFile(f: File): Option[Array[Char]] = f match {
    case RegularFile(_) => f.lines.headOption.map(_.toCharArray)
    case _ => None
  }

  object Implicits {

    implicit class ConfigOps(cfg: Config) {

      def getFile(key: String): File = File(cfg.getString(key))

      def getCommand(key: String): List[String] =
        cfg.getString(key).split("\\s+").toList

      def getIndexDb = getFile("chee.dbfile")

      def getSystemConfig = ConfigFile(getFile("chee.system-config"))
      def getLocationConf = new LocationConf(getSystemConfig)
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
        CheeConf.readPasswordFromFile(getFile(key))

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
    }
  }

  lazy val userConfig =
    findConfigFile
      .map(f => {
        if (debug) {
          System.err.println(s"Reading user config `${f.path}'")
        }
        ConfigFactory.parseFile(f.path.toFile)
      }).getOrElse(ConfigFactory.empty())

  lazy val defaultConfig = ConfigFactory.load()

  lazy val config = {
    val cfg = userConfig.withFallback(defaultConfig)
    setTempDir(cfg)
    cfg
  }

  lazy val userHome: File =
    File(System.getProperty("user.home"))

  def systemConfig: ConfigFile = {
    val file = userHome / ".config" / "chee" / "system.cfg"
    ConfigFile(file)
  }

  private def setTempDir(cfg: Config): Unit = {
    val tmpdir = cfg.getString("chee.tmpdir")
    if (tmpdir.nonEmpty) {
      val tmp = File(tmpdir)
      System.setProperty("java.io.tmpdir", tmp.path.toString)
      tmp.createIfNotExists(asDirectory = true)
    }
  }

  private def findConfigFile: Option[File] = {
    //system-property overrides, even if non existent
    val sys = Option(System.getProperty("chee.config")).map(File(_))
    if (sys.isDefined) {
      if (debug) {
        System.err.println(s"Use config file from system property: `${sys.get.path}'" +
          (if (!sys.forall(_.exists)) " (it doesn't exists)." else ""))
      }
      sys.filter(_.exists)
    } else {
      // then chee.conf in home dir
      val f1 = userHome / ".chee.conf"
      // then standard location
      val f2 = userHome / ".config" / "chee" / "chee.conf"
      Option(f1).filter(_.exists) orElse {
        Option(f2).filter(_.exists)
      }
    }
  }
}
