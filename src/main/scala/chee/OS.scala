package chee

import java.lang.{ProcessBuilder => JProcessBuilder}
import better.files.File
import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import java.awt.Desktop
import java.net.URI
import java.util.concurrent.atomic.AtomicReference
import scala.sys.process.{ Process, ProcessBuilder, ProcessLogger }
import scala.util.{ Failure, Success, Try }

trait OS {
  type Result = Try[Boolean]

  def edit(file: File): Result

  def browse(thing: String): Result

  def browse(file: File): Result =
    browse(file.pathAsString)

  def open(thing: String): Result

  def open(file: File): Result =
    open(file.pathAsString)

  def open(uri: URI): Result =
    open(uri.toString)

  implicit class ResultOps(r: Result) {
    def nextTry(n: => Result): Result = r match {
      case Success(false) => n
      case _ => r
    }
  }
}

object OS {

  sealed trait Type
  object Type {
    case object Linux extends Type
    case object Windows extends Type
    case object Osx extends Type
    case object Unknown extends Type
  }

  lazy val Current = System.getProperty("os.name", "").toLowerCase() match {
    case n if n contains "win" => Type.Windows
    case n if n contains "mac" => Type.Osx
    case n if n contains "linux" => Type.Linux
    case n if n contains "unix" => Type.Linux
    case _ => Type.Unknown
  }

  def apply(cfg: Config): OS =
    new OSImpl(new ConfigBacked(cfg), Current)

  object Command extends LazyLogging {
    /** Split a string into words, supporting single-quoted words. Replace
      * the last %s with `args`. */
    def apply(str: String, args: Seq[String] = Seq.empty): Try[Seq[String]] = Try {
       val ts = Tokenizer(str).fold(
        msg => UserError(s"Wrong command string `$str': $msg"),
        identity)
      ts.lastIndexOf("%s") match {
        case -1 => ts
        case n => ts.patch(n, args, 1)
      }
    }

    /** Split a string into words, supporting single-quoted words. Replace
      * the last %s with `arg +: args`. */
    def apply(str: String, arg: String, args: String*): Try[Seq[String]] =
      apply(str, arg +: args)

    private object Tokenizer {
      import fastparse.all._
      import chee.util.parsing._

      val simpleWord: P[String] = CharNotIn("' \t").rep(1).!
      val word: P[String] = quotedString(''')
      val token: P[Seq[String]] = P(WS.rep ~ (word | simpleWord) ~ WS.rep).rep

      def apply(s: String): Either[String, Seq[String]] =
        token.parseAll(s.trim)
    }
  }

  object Run extends LazyLogging {
    private def checkRc(rc: => Int): Try[Unit] =
      Try(rc).flatMap {
        case 0 => Success(())
        case n => Failure(new Exception(s"Non-zero exit code: $n"))
      }

    def exec(cmd: ProcessBuilder): Try[Unit] = checkRc( cmd.! )

    def exec(cmd: Seq[String]): Try[Unit] = {
      if (cmd.isEmpty) Try(UserError(s"Empty command string: `$cmd'"))
      else {
        logger.debug(s"Run command: $cmd")
        exec(Process(cmd))
      }
    }

    def exec(cmd: String, args: String*): Try[Unit] =
      exec(cmd +: args)

    /** Run `cmd` and return first line of stdout */
    def firstLine(cmd: Seq[String]): Try[String] =
      if (cmd.isEmpty) Try(UserError(s"Empty command string: `$cmd'"))
      else {
        logger.debug(s"Run command: $cmd")
        val line = new AtomicReference[String]()
        checkRc(Process(cmd) !< ProcessLogger(line.compareAndSet(null, _), Console.err.println)).map {
          _ => line.get
        }
      }

    // connect stdin/out/err to make terminal editors work
    def connected(cmd: Seq[String]) = {
      if (cmd.isEmpty) Try(UserError(s"Empty command string: `$cmd'"))
      logger.debug(s"Run command: $cmd")
      import scala.collection.JavaConverters._
      checkRc {
        val pb = new JProcessBuilder(cmd.asJava);
        pb.redirectOutput(JProcessBuilder.Redirect.INHERIT);
        pb.redirectError(JProcessBuilder.Redirect.INHERIT);
        pb.redirectInput(JProcessBuilder.Redirect.INHERIT);
        pb.start().waitFor()
      }
    }
  }

  final class ConfigBacked(cfg: Config) extends OS with LazyLogging {
    def browse(thing: String): Result = cfg.getString("chee.programs.browser") match {
      case c if c.nonEmpty =>
        logger.debug(s"Use browse command from config: $c")
        Command(c, thing).flatMap(Run.exec).map(_ => true)
      case _ => Success(false)
    }

    def edit(file: File): Result = cfg.getString("chee.programs.editor") match {
      case c if c.nonEmpty =>
        logger.debug(s"Use edit command from config: $c")
        Command(c, file.pathAsString).flatMap(Run.connected).map(_ => true)
      case _ => Success(false)
    }

    def open(thing: String): Result = Success(false)
  }

  object JavaDesktop extends OS with LazyLogging {
    private lazy val desktop: Option[Desktop] =
      if (Desktop.isDesktopSupported()) Some(Desktop.getDesktop)
      else {
        logger.debug("Java Desktop Api not supported on this platform")
        None
      }

    private def recover(body: => Any): Try[Boolean] =
      Try(body).map(_ => true).recover {
        case ex =>
          logger.debug("Error using java desktop api", ex)
          false
      }

    def edit(file: File): Result = desktop match {
      case Some(d) =>
        logger.debug(s"Use java desktop api to edit `${file.path}'.")
        recover(d.edit(file.toJava))
      case None => Success(false)
    }

    def browse(thing: String): Result = desktop match {
      case Some(d) =>
        logger.debug(s"Use java desktop api to browse `$thing'.")
        recover(d.browse(URI.create(thing)))
      case None => Success(false)
    }

    def open(thing: String): Result = desktop match {
      case Some(d) =>
        logger.debug(s"Use java desktop api to open `$thing'.")
        recover(d.open(new java.io.File(thing)))
      case None => Success(false)
    }
  }

  final class OSImpl(cfg: ConfigBacked, os: Type) extends OS {
    private def openFile(file: String): Result = os match {
      case Type.Linux =>
        Run.exec("kde-open", file)
          .orElse(Run.exec("gnome-open", file))
          .orElse(Run.exec("xdg-open", file))
          .orElse(JavaDesktop.open(file))
          .map(_ => true)
      case Type.Windows =>
        Run.exec("rundll32", "url.dll,FileProtocolHandler", file)
          .orElse(JavaDesktop.open(file))
          .map(_ => true)
      case Type.Osx =>
        Run.exec("open", file).orElse(JavaDesktop.open(file)).map(_ => true)
      case Type.Unknown =>
        JavaDesktop.open(file)
    }

    def edit(file: File): Result =
      cfg.edit(file)
        .nextTry(JavaDesktop.edit(file))
        .nextTry(open(file.pathAsString))

    def browse(thing: String): Result =
      cfg.browse(thing)
        .nextTry(JavaDesktop.browse(thing))
        .nextTry(open(thing))

    def open(thing: String): Result =
      cfg.open(thing) nextTry openFile(thing)
  }
}
