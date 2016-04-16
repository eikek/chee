package chee.sbt

import org.stringtemplate.v4._
import org.stringtemplate.v4.misc._

/** Helper for stringtemplate */
trait Template {
  type Context = Map[String, Any]

  def render(context: Context): String
  def write(file: sbt.File, context: Context): Int
}

object Template {

  def apply(template: String): Template =
    new Impl(template)

  def apply(f: sbt.File): Template =
    apply(sbt.IO.read(f))

  class Impl(template: String) extends Template {

    private val errorListener = new STErrorListener {
      def compileTimeError(msg: STMessage): Unit = sys.error(msg.toString)
      def internalError(msg: STMessage): Unit = sys.error(msg.toString)
      def IOError(msg: STMessage): Unit = sys.error(msg.toString)
      def runTimeError(msg: STMessage): Unit = sys.error(msg.toString)
    }

    def makeST(context: Context): ST = {
      val st = new ST(template)
      context.foldLeft(st) { case (t, (k, v)) =>
        t.add(k, v)
      }
    }

    def render(context: Context): String =
      makeST(context).render

    def write(file: sbt.File, context: Context): Int =
      makeST(context).write(file, errorListener, "UTF-8")
  }
}
