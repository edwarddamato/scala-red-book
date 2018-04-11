package Chapter3

import org.scalatest.{FlatSpec, Matchers}

class LiztTest extends FlatSpec with Matchers {
  // 3.1
  "some pattern matching" should "match expected result" in {
    val x = Lizt(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + Lizt.sum(t)
      case _ => 101
    }

    x shouldBe 3
  }

  "tail" should "remove first element in a list" in {
    Lizt.tail(Lizt(1,2,3,4,5)) shouldBe Lizt(2,3,4,5)
  }

  "setHead" should "replace the head for the given list" in {
    Lizt.setHead(Lizt(1,2,3,4,5), 55) shouldBe Lizt(55,2,3,4,5)
  }

  "drop" should "remove the first n elements from a list" in {
    Lizt.drop(Lizt(1,2,3,5,6,7,8), 3) shouldBe Lizt(5,6,7,8)
  }

  "dropWhile" should "remove elements from the List prefix as long as they match a predicate" in {
    Lizt.dropWhile(Lizt(1,2,3,4,5,6,7,8))((xv: Int) => xv <= 4) shouldBe Lizt(5,6,7,8)
  }

  "init" should "return a List consisting of all but the last element of a List" in {
    Lizt.init(Lizt(1,2,3,5,6,7)) shouldBe Lizt(1,2,3,5,6)
  }

  "foldRight" should "traverse left to right" in {
    Lizt.foldRight(Lizt(1,2,3,4,5), "")("%d,%s".format(_, _)) shouldBe "1,2,3,4,5,"
  }

  "Chapter3.map" should "foo" in {
    assert(Lizt.map(Lizt(1,2,3,4,5))((num:Int) => num + 5) == Lizt(6,7,8,9,10))
  }

  "Chapter3.filter" should "" in {
    assert(Lizt.filter(Lizt(1,2,3,4,5))((num:Int) => num % 2 == 0) == Lizt(2,4))
  }

  "Chapter3.flatMap" should "" in {
    assert(Lizt.flatMap(Lizt(1,2,3,4,5))((num:Int) => Lizt(num, num)) == Lizt(1,1,2,2,3,3,4,4,5,5))
  }

  "Chapter3.filterWithFlatMap" should "" in {
    assert(Lizt.filterWithFlatMap(Lizt(1,2,3,4,5))((num:Int) => num % 2 == 0) == Lizt(2,4))
  }

  "Map" should "map stuff" in {
    Lizt.map(Lizt(1,2,3))(_ + 1) shouldBe Lizt(2,3,4)
  }

  "addListElements" should "add elements in list" in {
    Lizt.addListElements(Lizt(1,2,3), Lizt(1,2,3)) shouldBe Lizt(2,4,6)
  }

  "zipWith" should "add elements in list" in {
    Lizt.zipWith(Lizt(1,2,3), Lizt(1,2,3))((n1: Int, n2: Int) => n1 * n2) shouldBe Lizt(1,4,9)
  }

  "hasSubsequence" should "check for subsequence" in {
    Lizt.hasSubsequence(List(1,2,3,5), List(1)) shouldBe true
    Lizt.hasSubsequence(List(1,2,3,5), List(1,2)) shouldBe true
    Lizt.hasSubsequence(List(1,2,3,5), List(1,3)) shouldBe false
    Lizt.hasSubsequence(List(1,2,3,5), List(5)) shouldBe true
    Lizt.hasSubsequence(List(1,2,3,5), List(3)) shouldBe true
    Lizt.hasSubsequence(List(1,2,3,5), List(1,2,3)) shouldBe true
    Lizt.hasSubsequence(List(1,2,3,5), List(2,3)) shouldBe true
    Lizt.hasSubsequence(List(1,2,3,5), List(2,3,4)) shouldBe false
    Lizt.hasSubsequence(List(1,2,3,5), List(2,3,5)) shouldBe true
  }
}
