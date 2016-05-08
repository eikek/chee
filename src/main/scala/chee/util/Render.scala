package chee.util

trait Render[A] {
  def render(a: A): String
}

object Render {
  def apply[A](f: A => String): Render[A] =
    new Render[A] {
      def render(a: A) = f(a)
    }

  implicit class Ops[A](el: A) {
    def render(implicit renderer: Render[A]): String =
      renderer.render(el)
  }

  object primitives {

    implicit val _stringRender: Render[String] = Render(identity)
    implicit val _intRender: Render[Int] = Render(_.toString)
    implicit val _longRender: Render[Long] = Render(_.toString)
    implicit val _boolRender: Render[Boolean] = Render(_.toString)
    implicit val _charRender: Render[Char] = Render(_.toString)
  }
}
