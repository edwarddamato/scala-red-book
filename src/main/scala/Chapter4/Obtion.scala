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