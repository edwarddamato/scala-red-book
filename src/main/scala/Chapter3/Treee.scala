package Chapter3

sealed trait Treee[+A]
case class Leaf[A](value: A) extends Treee[A]
case class Branch[A](left: Treee[A], right: Treee[A]) extends Treee[A]

object Treee {
  def apply[A](left: Treee[A], right: Treee[A]): Treee[A] = // Variadic function syntax
    Branch(left, right)

  def size[A](t: Treee[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Treee[Int]): Int = t match {
    case Leaf(num) => num
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Treee[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
  }

  def map[A, B](t: Treee[A])(f: A => B): Treee[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
}