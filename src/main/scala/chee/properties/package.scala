package chee

import scala.util.{Try, Success, Failure}

package object properties {

  implicit class TryOps[A](t: Try[A]) {
    def toEither(msg: String): Either[String, A] = t match {
      case Success(a) => Right(a)
      case Failure(e) => Left(msg + e.getMessage)
    }

    def toEither: Either[String, A] = toEither("")
  }

  def flatten2[A, B](es: Seq[(Either[A, B], Either[A, B])]): Either[A, List[(B, B)]] = {
    val zero: Either[A, List[(B, B)]] = Right(Nil)
    es.toStream.foldRight(zero) { case ((ea, eb), acc) =>
      if (acc.isLeft) acc
      else if (eb.isLeft) Left(eb.left.get)
      else if (ea.isLeft) Left(ea.left.get)
      else Right((ea.right.get, eb.right.get) :: acc.right.get)
    }
  }

  object Parallelism {
    // note, this is copy&paste from scala's ExecutionContextImpl to know the effectively
    // used pool size
    private def getInt(name: String, default: String) = (try System.getProperty(name, default) catch {
      case e: SecurityException => default
    }) match {
      case s if s.charAt(0) == 'x' => (Runtime.getRuntime.availableProcessors * s.substring(1).toDouble).ceil.toInt
      case other => other.toInt
    }

    private def range(floor: Int, desired: Int, ceiling: Int) = scala.math.min(scala.math.max(floor, desired), ceiling)

    val globalParallelism = range(
      getInt("scala.concurrent.context.minThreads", "1"),
      getInt("scala.concurrent.context.numThreads", "x1"),
      getInt("scala.concurrent.context.maxThreads", "x1"))
  }

}
