package chee.util

object predicates {

  def not[A](p: A => Boolean): A => Boolean =
    a => !p(a)

}
