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
}
