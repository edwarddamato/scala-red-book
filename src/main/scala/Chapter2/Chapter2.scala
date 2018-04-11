package Chapter2

object Chapter2 {
  // 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n1: Int, n2: Int, i: Int): Int = {
      if (i >= n) n1
      else go(n2, n1+n2, i+1)
    }

    go(0, 1, 1)
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean = {
      if (n >= as.length-1) true
      else ordered(as(n), as(n+1)) && go(n+1)
    }

    go(0)
  }

  // 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => ((b: B) => f(a, b))
  }

  // 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n-1, n*acc)
    }
    go(n, 1)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  private def formatResult(name: String, n: Int, f: Int => Int): Unit = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(isSorted(Array(1,2,3,4,5), ordered))

    val foo = curry((x: Int, y: Int) => x + y)
    println(uncurry(foo)(5, 101))
  }

  private def ordered[A](a1: A, a2: A): Boolean = {
    a1.asInstanceOf[Int] < a2.asInstanceOf[Int]
  }
}