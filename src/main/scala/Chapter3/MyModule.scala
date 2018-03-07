sealed trait Lizt[+A]
case object Nil extends Lizt[Nothing]

case class Cons[+A](head: A, tail: Lizt[A]) extends Lizt[A]

object Lizt {
  def sum(ints: Lizt[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: Lizt[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def product2(list: Lizt[Double]): Double = list match {
    case Nil => 0.0
    case _ => foldRight(list, 1.0)(_ * _)
  }

  def apply[A](as: A*): Lizt[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def main(args: Array[String]): Unit = {
//    println(sum2(Lizt(1,2,3,4,5)))
//    println(sumUsingFoldLeft(Lizt(1,2,3,4,5)))
//    println(lengthUsingFoldLeft(Lizt(1,2,3,4,5)))
//    println(concatLists(Lizt(Lizt(1,2,4), Lizt(51,1,2))))

//    println(incrementInts(Lizt(1,2,5,6,7), 1))

//    println(map(Lizt(1,2,3,5,6))((num:Int) => num + 5))
    println(doubleToString(Lizt(1.2,5.3,4.2)))
  }


  def tail[A](list: Lizt[A]): Lizt[A] = {
    list match {
      case Nil => list
      case Cons(_, tail) => tail
    }
  }

  def setHead[A](list: Lizt[A], head: A): Lizt[A] = {
    list match {
      case Cons(_, tail) => Cons(head, tail)
    }
  }

  def length[A](list: Lizt[A]): Int = {
    @annotation.tailrec
    def go[A](l: Lizt[A], len: Int): Int = {
      l match {
        case Cons(_, Nil) => len + 1
        case _ => go(Lizt.tail(l), len + 1)
      }
    }

    go(list, 0)
  }

  def length2[A](list: Lizt[A]): Int = list match {
    case Nil => 0
    case _ => foldRight(list, 0)((_,acc) => acc + 1)
  }

  def sum2(list: Lizt[Int]): Int = list match {
    case Nil => 0
    case _ => foldRight(list, 1)((x, y) => x + y)
  }

  /*
  Cons(1, Cons(2, Cons(3)))

   */

  def sumUsingFoldLeft(list: Lizt[Int]): Int = list match {
    case Nil => 0
    case _ => foldLeft(list, 1)(_ + _)
  }

  def productUsingFoldLeft(list: Lizt[Int]): Int = list match {
    case Nil => 0
    case _ => foldLeft(list, 1)(_ * _)
  }

  def lengthUsingFoldLeft(list: Lizt[Int]): Int = list match {
    case Nil => 0
    case _ => foldLeft(list, 0)((acc, _) => acc + 1)
  }

  def append[A](list: Lizt[A], extra: Lizt[A]): Lizt[A] = list match {
    case Nil => list
    case _ => foldRight(list, extra)((l, r) => Cons(l, r))
  }

  def concatLists[A](list: Lizt[Lizt[A]]): Lizt[A] = {
    foldRight(list, Lizt[A]())((h, l) => append(h, l))
  }

  def foldLeft[A, B](list: Lizt[A], z: B)(f: (B, A) => B): B = list match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  def incrementInts(list: Lizt[Int], inc: Int) = list match {
    case Nil => list
    case _ => foldRight(list, Lizt[Int]())((h, t) => Cons(h+inc, t))
  }

  def doubleToString(list: Lizt[Double]): String = list match {
    case Nil => ""
    case _ => foldRight(list, "")((h, t) => s"$h$t")
  }

  def map[A,B](list: Lizt[A])(f: A => B): Lizt[B] = list match {
    case Nil => Lizt[B]()
    case _ => foldRight(list, Lizt[B]())((h, t) => Cons(f(h), t))
  }

  /*
  Cons(1, Cons(2, Cons(3)))
  1 + Cons(2, Cons(3))
  1 + 2 + Cons(3, Nil)
  1 + 2 + 3
   */

  def foldRight[A, B](list: Lizt[A], z: B)(f: (A, B) => B): B = list match {
    case Nil => z
    case Cons(head, tail) => f(head, foldRight(tail, z)(f))
  }

  def reverse[A](list: Lizt[A]): Lizt[A] = list match {
    case Nil => Nil
    case _ => foldLeft(list, Lizt[A]())((l, h) => Cons(h, l))
  }


  /*
  Cons(1, Cons(2, Cons(3, Cons(4, Cons(5)))))
  1, []
  2, 1, []
  3, 2, 1, []
   */


  def drop[A](list: Lizt[A], n: Int): Lizt[A] = {
    val endListLength = Lizt.length(list) - n
    def go[A](l: Lizt[A]): Lizt[A] = {
      if (Lizt.length(l) == endListLength) l
      else go(Lizt.tail(l))
    }

    go(list)
  }

  def dropWhile[A](list: Lizt[A], f: A => Boolean): Lizt[A] = {
    list match {
      case Cons(head, tail) if f(head) => dropWhile(tail, f)
      case _ => list
    }
  }

  def init[A](list: Lizt[A]): Lizt[A] = {
    list match {
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }
  }

}
