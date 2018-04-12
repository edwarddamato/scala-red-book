package Chapter4

import org.scalatest.{FlatSpec, Matchers}

class EiderTest extends FlatSpec with Matchers {
  "map" should "map" in {
    val foo: Eider[String, Int] = Right(55)
    foo.map(_ * 2) shouldBe Right(110)
  }
}
