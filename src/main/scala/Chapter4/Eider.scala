package Chapter4

sealed trait Eider[+E, +A] {
  def map[B](f: A => B): Eider[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Eider[EE, B]): Eider[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Eider[EE, B]): Eider[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Eider[EE, B])(f: (A, B) => C): Eider[EE, C] = {
    this.flatMap(aa => b.map(bb => f(aa, bb)))
  }
}
case class Left[+E](value: E) extends Eider[E, Nothing]
case class Right[+A](value: A) extends Eider[Nothing, A]

object Eider {
  def traverse[E,A,B](es: List[A])(f: A => Eider[E, B]): Eider[E, List[B]] = es match {
    case Nil => Right(Nil)
    case h :: t => traverse(t)(f).flatMap(tt => f(h).map(hh => hh :: tt))
  }

  def sequence[E, A](es: List[Eider[E, A]]): Eider[E, List[A]] =
    traverse(es)(identity)
}