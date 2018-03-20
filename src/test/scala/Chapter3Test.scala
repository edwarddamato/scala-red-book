import org.scalatest.{FlatSpec, FunSuite, Matchers}

class Chapter3Test extends FunSuite {
  test("Chapter3.map") {
    assert(Lizt.map(Lizt(1,2,3,4,5))((num:Int) => num + 5) == Lizt(6,7,8,9,10))
  }

  test("Chapter3.filter") {
    assert(Lizt.filter(Lizt(1,2,3,4,5))((num:Int) => num % 2 == 0) == Lizt(2,4))
  }

  test("Chapter3.flatMap") {
    assert(Lizt.flatMap(Lizt(1,2,3,4,5))((num:Int) => Lizt(num, num)) == Lizt(1,1,2,2,3,3,4,4,5,5))
  }

  test("Chapter3.filterWithFlatMap") {
    assert(Lizt.filterWithFlatMap(Lizt(1,2,3,4,5))((num:Int) => num % 2 == 0) == Lizt(2,4))
  }
}

class BlaTest extends FlatSpec with Matchers {
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

  "size" should "get tree size" in {
    Treee.size(Treee(Branch(Branch(Leaf("moo"), Leaf("cow")), Branch(Leaf("aaa"), Leaf("baa"))), Branch(Leaf("foo"), Leaf("bar")))) shouldBe 11
  }

  "maximum" should "return the maximum" in {
    Treee.maximum(
      Treee(
        Branch(
          Branch(
            Leaf(1),
            Leaf(5)
          ),
          Branch(
            Leaf(16),
            Leaf(2)
          )
        ),
        Branch(
          Leaf(10),
          Leaf(5)
        )
      )) shouldBe 16
  }

  "depth" should "return the depth" in {
    Treee.depth(
      Treee(
        Branch(
          Branch(
            Leaf(1),
            Leaf(5)
          ),
          Branch(
            Leaf(16),
            Leaf(2)
          )
        ),
        Branch(
          Leaf(10),
          Leaf(5)
        )
      )) shouldBe 4
  }

  "map" should "map a tree" in {
    Treee.map(
      Treee(
        Branch(
          Branch(
            Leaf(1),
            Leaf(2)
          ),
          Branch(
            Leaf(3),
            Leaf(4)
          )
        ),
        Branch(
          Leaf(5),
          Leaf(6)
        )
      ))(x => x * 2) shouldBe
      Treee(
        Branch(
          Branch(
            Leaf(2),
            Leaf(4)
          ),
          Branch(
            Leaf(6),
            Leaf(8)
          )
        ),
        Branch(
          Leaf(10),
          Leaf(12)
        )
      )
  }
}