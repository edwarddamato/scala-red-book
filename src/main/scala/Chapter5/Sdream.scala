package Chapter5

sealed trait Sdream[+A] {
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, Empty) => List(h())
    case Cons(h, t) => h() :: t().toList
  }
}
case object Empty extends Sdream[Nothing]
case class Cons[+A](h: () => A, t: () => Sdream[A]) extends Sdream[A]

