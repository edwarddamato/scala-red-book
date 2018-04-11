sealed trait Obtion[+A] {
  def map[B](f: A => B): Obtion[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Obtion[B]): Obtion[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Obtion[B]): Obtion[B] = this match {
    case None => ob
    case Some(_) => this
  }

  def filter(f: A => Boolean): Obtion[A] = this match {
    case None => None
    case Some(a) => if (f(a)) Some(a) else None
  }
}
case class Some[+A](get: A) extends Obtion[A]
case object None extends Obtion[Nothing]

object SeqOperations {
  def mean(xs: Seq[Double]): Obtion[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Obtion[Double] = {
    if (xs.isEmpty) None
    else mean(xs).flatMap(m => mean(xs.map(v => Math.pow(v - m, 2))))
  }

  def map2[A,B,C](a: Obtion[A], b: Obtion[B])(f: (A, B) => C): Obtion[C] = {
    a.flatMap(aa => b.flatMap(bb => Some(f(aa, bb))))
  }

  def sequence[A](a: List[Obtion[A]]): Obtion[List[A]] = a match {
    case Nil => Some(Nil)
    case None :: _ => None
    case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Obtion[B]): Obtion[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => traverse(t)(f).flatMap(tt => f(h).map(hh => hh :: tt))
  }

  def sequenceWithTraverse[A](a: List[Obtion[A]]): Obtion[List[A]] = traverse(a)(identity)


  def ANSWERtraverse[A, B](a: List[A])(f: A => Obtion[B]): Obtion[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    }
}