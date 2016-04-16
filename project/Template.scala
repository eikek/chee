package chee.sbt

import org.stringtemplate.v4._
import org.stringtemplate.v4.misc._
import scala.util.{ Try, Success, Failure }

/** Helper for stringtemplate */
trait Template {
  type Context = Map[String, Any]

  def render(context: Context): String
  def write(file: sbt.File, context: Context): Int
}

object Template {

  def apply(template: String, filename: Option[String] = None): Template =
    new Impl(template, filename)

  def apply(f: sbt.File): Template =
    apply(sbt.IO.read(f), Some(f.getName))

  class Impl(template: String, filename: Option[String] = None) extends Template {

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

    private def run[T](body: => T): T =
      Try(body) match {
        case Success(t) => t
        case Failure(ex) => throw new Exception(s"Error in file $filename", ex)
      }

    def render(context: Context): String =
      run(makeST(context).render)

    def write(file: sbt.File, context: Context): Int =
      run(makeST(context).write(file, errorListener, "UTF-8"))
  }
}
