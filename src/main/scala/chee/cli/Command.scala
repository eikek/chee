package chee.cli

import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging

sealed trait CommandTree {
  def name: String

  def toPath: List[CommandTree.Path] = this match {
    case cmd: Command => List(List(cmd.name))
    case HubCommand(name, children) => children.map(c => List(name, c.name))
  }
}

object CommandTree {
  type Path = List[String]

  def toPaths(trees: List[CommandTree]): List[Path] =
    trees.flatMap(_.toPath)
}

trait Command extends CommandTree {
  def exec(cfg: Config, args: Array[String]): Unit

  def outln(s: String): Unit =
    println(s)

  def out(s: String): Unit =
    print(s)

  def errln(s: String): Unit =
    Console.err.println(s)
}

case class HubCommand(name: String, children: List[CommandTree]) extends CommandTree

object Command {
  private def commandList(tree: List[CommandTree], cmd: Option[String]): String = {
    val paths = CommandTree.toPaths(tree)
    cmd match {
      case None =>
        chee.CheeDoc.formatCommandSummary(paths)
      case Some(name) =>
        chee.CheeDoc.formatCommandSummary(paths.map(p => name :: p))
    }
  }

  def find(names: Array[String], cmds: List[CommandTree]): Either[String, (Array[String], Command)] = {
    @scala.annotation.tailrec
    def loop(path: Array[String], cmds: List[CommandTree], cmd: Option[String] = None): Either[String, (Array[String], Command)] =
      path.headOption match {
        case None =>
          Left("A command is required! "
            + s"""Possible commands are:\n\n${commandList(cmds, cmd)}""")
        case Some(name) =>
          cmds.filter(_.name.startsWith(name)) match {
            case (cmd: Command) :: Nil =>
              Right((path.tail, cmd))
            case HubCommand(name, subs) :: Nil =>
              loop(path.tail, subs, Some(name))
            case Nil =>
              Left(s"Command not found `$name'. "
                + s"""Possible commands are:\n\n${commandList(cmds, cmd)}""")
            case candidates =>
              Left(s"Ambiguous name `${name}'. " +
                s"""Possible candidates are:\n\n${commandList(candidates, cmd)}.""")
          }
      }

    loop(names, cmds)
  }
}

trait ScoptCommand extends Command with LazyLogging {

  type T

  def parser: scopt.OptionParser[T]
  def defaults: T
  def exec(cfg: Config, opts: T): Unit

  abstract class Parser extends CheeOptionParser[T](name) {
    override def showTryHelp(): Unit = {
      errln(cheeTryHelp)
    }

    override def reportWarning(msg: String): Unit = {
      errln(s"Warning: $msg")
    }

    override def reportError(msg: String): Unit = {
      errln(s"Error: $msg")
    }
  }

  final def exec(cfg: Config, args: Array[String]): Unit =
    parser.parse(args, defaults) match {
      case Some(t) =>
        logger.trace(s"Executing command $name with options $t")
        exec(cfg, t)
      case _ =>
        userError("Invalid options")
    }
}
