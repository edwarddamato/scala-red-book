package Chapter2

import org.scalatest.{FlatSpec, Matchers}

class Chapter2Test extends FlatSpec with Matchers {
  "abs" should "return the absolute value of the given integer" in {
    val absVal = Chapter2.abs(55)
    absVal shouldBe 55
  }

  it should "return a positive value of the given negative integer" in {
    val absVal = Chapter2.abs(-51)
    absVal shouldBe 51
  }

  "factorial" should "return the factorial value of the given integer" in {
    val factorial = Chapter2.factorial(5)
    factorial shouldBe 120
  }

  it should "return 5040 as the factorial of 7" in {
    Chapter2.factorial(7) shouldBe 5040
  }

  "fib" should "return the nth digit in the fibonacci sequence" in {
    Chapter2.fib(8) shouldBe 13
    Chapter2.fib(5) shouldBe 3
  }

  "isSorted" should "check whether an Array[A] is sorted according to a given comparison function" in {
    Chapter2.isSorted(Array(1, 2, 3, 4, 5), (x: Int, y: Int) => x > y) shouldBe false
    Chapter2.isSorted(Array(1, 2, 3, 4, 5), (x: Int, y: Int) => x < y) shouldBe true
  }

  "curry" should "convert a function f of two arguments into a function of one argument that partially applies f" in {
    def multiply(value: Int, multiplier: Int): Int = {
      value * multiplier
    }

    Chapter2.curry(multiply)(10)(5) shouldBe 50
  }

  "uncurry" should "reverse curry" in {
    def curriedMultiply(value: Int): (Int) => Int = {
      (multiplier: Int) => multiplier * value
    }

    curriedMultiply(3)(10) shouldBe 30
    Chapter2.uncurry(curriedMultiply)(3, 10) shouldBe 30
  }

  "compose" should "compose two functions" in {
    def triplicate(value: Int): Int = {
      value * 3
    }

    def duplicate(value: Int): Int = {
      value * 2
    }

    Chapter2.compose(duplicate, triplicate)(10) shouldBe 60
  }
}
