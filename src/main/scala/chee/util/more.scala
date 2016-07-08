package chee.util

object more {

  def sliced[A](first: Option[Int], skip: Option[Int]): Traversable[A] => Traversable[A] = {
    def lift(f: (Traversable[A], Int) => Traversable[A]): Option[Int] => Traversable[A] => Traversable[A] =
      n => s => n.map(f(s, _)).getOrElse(s)

    lift(_ drop _)(skip) andThen lift(_ take _)(first)
  }

}
