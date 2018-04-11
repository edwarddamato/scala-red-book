package Chapter3

import org.scalatest.{FlatSpec, Matchers}

class TreeeTest extends FlatSpec with Matchers {
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
      ))(x => x * 2) shouldBe 4
  }
}
