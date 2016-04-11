package chee.cli

import java.time.Duration
import org.slf4j.{Logger, LoggerFactory}
import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.core.util.StatusPrinter
import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import chee.{CheeConf, UserError}
import chee.CheeConf.Implicits._
import chee.Timing

/** The main entry point to chee.
  * 
  * Sets up configuration and logging.
  */
object Main extends LazyLogging {

  val chee: List[CommandTree] = List(
    Help,
    Version,
    Find,
    Location.root,
    HubCommand("collection", List(CollectionEdit, CollectionShow)),
    View,
    MkTree,
    Thumb,
    Scale,
    ConfigCmd,
    Encrypt,
    Decrypt,
    Clean
  )

  def setupLogging(cfg: Config): Unit = {
    val logFile = cfg.getFile("chee.logConfig")
    if (logFile.exists) {
      val context = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
      scala.util.Try {
        val config = new JoranConfigurator()
        config.setContext(context)
        context.reset()
        config.doConfigure(logFile.path.toString)
      }
      StatusPrinter.printInCaseOfErrorsOrWarnings(context)
    }
  }

  def main(args: Array[String]): Unit = {
    val config = CheeConf.config
    setupLogging(config)
    Command.find(args, chee) match {
      case Left(msg) =>
        Console.err.println(msg)
        System.exit(100)
      case Right((rest, cmd)) =>
        try {
          logger.info(s"""chee starts with command: ${args.mkString(" ")}""")
          val (_, d) = Timing.timedResult(cmd.exec(config, rest))
          logger.info(s"""executed '${args.mkString(" ")}' in ${Timing.format(d)}""")
        } catch {
          case u: UserError =>
            Console.err.println("Error: " + u.getMessage)
            logger.error("user error!", u)
            System.exit(103)
          case e: Exception =>
            Console.err.println(s"System Failure: ${e.getMessage}")
            logger.error("system error!", e)
            System.exit(1)
        }
    }    
  }
}
