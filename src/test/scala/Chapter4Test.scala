/**
  * Created by eddamato on 19/03/2018.
  */
import org.scalatest.{FlatSpec, Matchers}

class Chapter4Test extends FlatSpec with Matchers {
  "map" should "map" in {
    val foo: Obtion[Int] = Some(5)
    foo.map(_ * 2) shouldBe Some(10)
  }

  "flatMap" should "flatMap" in {
    val foo: Obtion[List[Int]] = Some(List(1, 2, 3))
    foo.flatMap(list => Some(list)) shouldBe Some(List(1,2,3))
  }

  "getOrElse" should "getOrElse" in {
    val foo: Obtion[Int] = Some(5)
    foo.getOrElse(0) shouldBe 5

    val bar: Obtion[Int] = None
    bar.getOrElse(0) shouldBe 0
  }

  "orElse" should "orElse" in {
    val foo: Obtion[Int] = Some(5)
    foo.orElse(Some(0)) shouldBe Some(5)

    val bar: Obtion[Int] = None
    bar.orElse(Some(5)) shouldBe Some(5)
  }

  "filter" should "filter" in {
    val foo: Obtion[Int] = Some(5)
    foo.filter(x => x == 5) shouldBe Some(5)

    val bar: Obtion[Int] = Some(10)
    bar.filter(x => x == 5) shouldBe None
  }
}
