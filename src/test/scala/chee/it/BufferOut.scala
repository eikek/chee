package chee.it

import chee.cli.Command
import java.util.concurrent.atomic.AtomicReference

trait BufferOut extends Command {

  private val stdout = new AtomicReference[LineBuffer](LineBuffer())
  private val stderr = new AtomicReference[LineBuffer](LineBuffer())

  private def update(ref: AtomicReference[LineBuffer])(f: LineBuffer => LineBuffer): Unit = {
    val cur = ref.get
    val next = f(cur)
    if (!ref.compareAndSet(cur, next)) {
      update(ref)(f)
    }
  }

  override def outln(s: String): Unit = {
    update(stdout)(_ putln s)
  }

  override def errln(s: String): Unit = {
    update(stderr)(_ putln s)
  }

  override def out(s: String): Unit = {
    update(stdout)(_ put s)
  }

  def stdoutLines = stdout.get.lines.toList
  def stderrLines = stderr.get.lines.toList

}

case class LineBuffer(buffer: Vector[String] = Vector.empty, newLine: Boolean = false) {

  def putln(s: String): LineBuffer =
    put(s).put("\n")

  def put(s: String): LineBuffer =
    s.foldLeft(this) { (lb, c) =>
      lb putChar c
    }

  def putChar(c: Char): LineBuffer = c match {
    case '\n' if newLine => LineBuffer(buffer :+ "", false)
    case '\n' => LineBuffer(buffer, true)
    case _ if newLine || buffer.isEmpty =>
      LineBuffer(buffer :+ c.toString, false)
    case _ =>
      val (init, last) = (buffer.init, buffer.last)
      LineBuffer(init :+ (last + c.toString), newLine)
  }

  private def countTrailingNewlines(s: String): Int = {
    def loop(idx: Int, result: Int): Int =
      idx match {
        case i if i < 0 || i >= s.length => result
        case i if s.charAt(i) == '\n' => loop(i - 1, result + 1)
        case i => result
      }
    loop(s.length -1, 0)
  }

  lazy val lines = buffer
}
