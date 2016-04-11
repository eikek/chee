package chee.cli

import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging

sealed trait CommandTree {
  def name: String
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
  def find(names: Array[String], cmds: List[CommandTree]): Either[String, (Array[String], Command)] = {
    names.headOption match {
      case None =>
        Left("A command is required! "
          + s"""Possible commands are: ${cmds.map(_.name).mkString(", ")}""")
      case Some(name) =>
        cmds.filter(_.name.startsWith(name)) match {
          case (cmd: Command) :: Nil =>
            Right((names.tail, cmd))
          case HubCommand(_, subs) :: Nil =>
            find(names.tail, subs)
          case Nil =>
            Left(s"Command not found `$name'. "
              + s"""Possible commands are: ${cmds.map(_.name).mkString(", ")}""")
          case candidates =>
            Left(s"Ambiguous name `${name}'. " +
              s"""Possible candidates are: ${candidates.map(_.name).mkString(", ")}.""")
        }
    }
  }
}

trait ScoptCommand extends Command with LazyLogging {

  type T

  def parser: scopt.OptionParser[T]
  def defaults: T
  def exec(cfg: Config, opts: T): Unit

  abstract class Parser extends CheeOptionParser[T](name)

  final def exec(cfg: Config, args: Array[String]): Unit =
    parser.parse(args, defaults) match {
      case Some(t) =>
        logger.trace(s"Executing command $name with options $t")
        exec(cfg, t)
      case _ =>
        System.exit(105)
    }
}
