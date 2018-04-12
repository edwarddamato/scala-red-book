package Chapter3

sealed trait Lizt[+A]
case object Nill extends Lizt[Nothing]

case class Cons[+A](head: A, tail: Lizt[A]) extends Lizt[A]

object Lizt {
  def sum(ints: Lizt[Int]): Int = ints match {
    case Nill => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: Lizt[Double]): Double = ds match {
    case Nill => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def product2(list: Lizt[Double]): Double = list match {
    case Nill => 0.0
    case _ => foldRight(list, 1.0)(_ * _)
  }

  def apply[A](as: A*): Lizt[A] = // Variadic function syntax
    if (as.isEmpty) Nill
    else Cons(as.head, apply(as.tail: _*))

  //  def main(args: Array[String]): Unit = {
  ////    println(sum2(Lizt(1,2,3,4,5)))
  ////    println(sumUsingFoldLeft(Lizt(1,2,3,4,5)))
  ////    println(lengthUsingFoldLeft(Lizt(1,2,3,4,5)))
  ////    println(concatLists(Lizt(Lizt(1,2,4), Lizt(51,1,2))))
  //
  ////    println(incrementInts(Lizt(1,2,5,6,7), 1))
  //
  ////    println(map(Lizt(1,2,3,5,6))((num:Int) => num + 5))
  //    println(doubleToString(Lizt(1.2,5.3,4.2)))
  //  }

  // 3.2
  def tail[A](list: Lizt[A]): Lizt[A] = {
    list match {
      case Nill => list
      case Cons(_, tail) => tail
    }
  }

  // 3.3
  def setHead[A](list: Lizt[A], head: A): Lizt[A] = {
    list match {
      case Cons(_, tail) => Cons(head, tail)
    }
  }

  // 3.4
  def drop[A](list: Lizt[A], n: Int): Lizt[A] = {
    if (n == 0) list
    else list match {
      case Cons(_, tail) => drop(tail, n-1)
    }
  }

  // 3.5
  def dropWhile[A](list: Lizt[A])(f: A => Boolean): Lizt[A] = {
    list match {
      case Cons(head, tail) if f(head) => dropWhile(tail)(f)
      case _ => list
    }
  }

  // 3.6
  def init[A](list: Lizt[A]): Lizt[A] = {
    list match {
      case Cons(_, Nill) => Nill
      case Cons(head, tail) => Cons(head, init(tail))
    }
  }

  def foldRight[A, B](list: Lizt[A], z: B)(f: (A, B) => B): B = list match {
    case Nill => z
    case Cons(head, tail) => f(head, foldRight(tail, z)(f))
  }

  def length[A](list: Lizt[A]): Int = {
    @annotation.tailrec
    def go[A](l: Lizt[A], len: Int): Int = {
      l match {
        case Cons(_, Nill) => len + 1
        case _ => go(Lizt.tail(l), len + 1)
      }
    }

    go(list, 0)
  }

  def length2[A](list: Lizt[A]): Int = list match {
    case Nill => 0
    case _ => foldRight(list, 0)((_,acc) => acc + 1)
  }

  def sum2(list: Lizt[Int]): Int = list match {
    case Nill => 0
    case _ => foldRight(list, 1)((x, y) => x + y)
  }

  def sumUsingFoldLeft(list: Lizt[Int]): Int = list match {
    case Nill => 0
    case _ => foldLeft(list, 1)(_ + _)
  }

  def productUsingFoldLeft(list: Lizt[Int]): Int = list match {
    case Nill => 0
    case _ => foldLeft(list, 1)(_ * _)
  }

  def lengthUsingFoldLeft(list: Lizt[Int]): Int = list match {
    case Nill => 0
    case _ => foldLeft(list, 0)((acc, _) => acc + 1)
  }

  def append[A](list: Lizt[A], extra: Lizt[A]): Lizt[A] = list match {
    case Nill => list
    case _ => foldRight(list, extra)((l, r) => Cons(l, r))
  }

  def concatLists[A](list: Lizt[Lizt[A]]): Lizt[A] = {
    foldRight(list, Lizt[A]())((h, l) => append(h, l))
  }

  def foldLeft[A, B](list: Lizt[A], z: B)(f: (B, A) => B): B = list match {
    case Nill => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  def incrementInts(list: Lizt[Int], inc: Int) = list match {
    case Nill => list
    case _ => foldRight(list, Lizt[Int]())((h, t) => Cons(h+inc, t))
  }

  def doubleToString(list: Lizt[Double]): String = list match {
    case Nill => ""
    case _ => foldRight(list, "")((h, t) => s"$h$t")
  }

  def map[A,B](list: Lizt[A])(f: A => B): Lizt[B] = list match {
    case Nill => Lizt[B]()
    case _ => foldRight(list, Lizt[B]())((h, t) => Cons(f(h), t))
  }

  def filterReverse[A](list: Lizt[A])(f: A => Boolean): Lizt[A] = list match {
    case Nill => list
    case _ => foldLeft(list, Lizt[A]())((ls, value) => {
      if (f(value)) Cons(value, ls)
      else ls
    })
  }

  def filter[A](list: Lizt[A])(f: A => Boolean): Lizt[A] = list match {
    case Nill => list
    case _ => foldRight(list, Lizt[A]())((value, ls) => {
      if (f(value)) Cons(value, ls)
      else ls
    })
  }

  def flatMap[A,B](list: Lizt[A])(f: A => Lizt[B]): Lizt[B] = list match {
    case Nill => Lizt[B]()
    case _ => foldRight(list, Lizt[B]())((h, t) => {
      foldRight(f(h), t)((h, t) => Cons(h, t))
    })
  }

  def filterWithFlatMap[A](list: Lizt[A])(f: A => Boolean): Lizt[A] = list match {
    case Nill => list
    case _ => flatMap(list)((num:A) => {
      if (f(num)) Lizt(num)
      else Nill
    })
  }

  def addListElements(list1: Lizt[Int], list2: Lizt[Int]): Lizt[Int] = (list1, list2) match {
    case (Nill, _) => list2
    case (_, Nill) => list1
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addListElements(t1, t2))
  }

  def zipWith[A, B, C](list1: Lizt[A], list2: Lizt[B])(f: (A,B) => C): Lizt[C] = (list1, list2) match {
    case (Nill, _) => Nill
    case (_, Nill) => Nill
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case List() => false
    case _ :: t => {
      if (sup.startsWith(sub)) true
      else {
        hasSubsequence(t, sub)
      }
    }
  }

  def reverse[A](list: Lizt[A]): Lizt[A] = list match {
    case Nill => Nill
    case _ => foldLeft(list, Lizt[A]())((l, h) => Cons(h, l))
  }
}
