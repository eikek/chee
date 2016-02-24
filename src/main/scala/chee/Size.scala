package chee

case class Size(width: Int, height: Int)

object Size {
  def apply(n: Int): Size = Size(n, n)
}
