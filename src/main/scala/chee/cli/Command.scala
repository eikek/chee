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
  private def commandList(tree: List[CommandTree]): List[String] = {
    @scala.annotation.tailrec
    def loop(stack: List[(String, CommandTree)], result: List[String] = Nil): List[String] =
      stack match {
        case Nil =>
          result.reverse
        case (name, h: HubCommand) :: cs =>
          loop(h.children.sortBy(_.name).map(c => (s"${h.name} ${c.name}" -> c)) ::: cs, result)
        case (name, _) :: cs =>
          loop(cs, name :: result)
      }
    loop(tree.sortBy(_.name).map(t => (t.name -> t)))
  }

  def find(names: Array[String], cmds: List[CommandTree]): Either[String, (Array[String], Command)] = {
    names.headOption match {
      case None =>
        Left("A command is required! "
          + s"""Possible commands are: ${commandList(cmds).mkString(", ")}""")
      case Some(name) =>
        cmds.filter(_.name.startsWith(name)) match {
          case (cmd: Command) :: Nil =>
            Right((names.tail, cmd))
          case HubCommand(_, subs) :: Nil =>
            find(names.tail, subs)
          case Nil =>
            Left(s"Command not found `$name'. "
              + s"""Possible commands are: ${commandList(cmds).mkString(", ")}""")
          case candidates =>
            Left(s"Ambiguous name `${name}'. " +
              s"""Possible candidates are: ${commandList(candidates).mkString(", ")}.""")
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
