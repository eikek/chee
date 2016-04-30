package chee.it

import chee.cli.Command

trait BufferOut extends Command {

  private val stdout = new collection.mutable.ListBuffer[String]()
  private val stderr = new collection.mutable.ListBuffer[String]()

  override def outln(s: String): Unit = {
    stdout append s
  }

  override def errln(s: String): Unit = {
    stderr append s
  }

  override def out(s: String): Unit = {
    val lines = s.split("\n")
    if (stdout.isEmpty) lines.foreach(stdout.append(_))
    else {
      val last = stdout.remove(stdout.length -1)
      stdout append (last + lines.head)
      lines.tail.foreach(stdout.append(_))
    }
    if (s endsWith "\n") {
      stdout append ""
    }
  }

  def stdoutLines = stdout.toList
  def stderrLines = stderr.toList

}
