package Chapter4

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

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

  "variance" should "variance" in {
    SeqOperations.variance(Seq(1, 2, 3, 4)) shouldBe Some(1.25)
    SeqOperations.varianceForComp(Seq(1, 2, 3, 4)) shouldBe Some(1.25)
  }

  "map2" should "map2" in {
    SeqOperations.map2(Some(1), Some(2))(_ + _) shouldBe Some(3)
    SeqOperations.map2(Some(1), None)((a, b) => "foo") shouldBe None
    SeqOperations.map2(None, Some(2))((a, b) => "foo") shouldBe None
    SeqOperations.map2(None, None)((a, b) => "foo") shouldBe None
  }

  "sequence" should "sequence" in {
    SeqOperations.sequence(List(Some(1), Some(2))) shouldBe Some(1 :: 2 :: Nil)
    SeqOperations.sequence(List(Some(1), Some(2), None)) shouldBe None
    SeqOperations.sequence(Nil) shouldBe Some(List())
  }

  "sequenceWithTraverse" should "sequenceWithTraverse" in {
    SeqOperations.sequenceWithTraverse(List(Some(1), Some(2))) shouldBe Some(1 :: 2 :: Nil)
    SeqOperations.sequenceWithTraverse(List(Some(1), Some(2), None)) shouldBe None
    SeqOperations.sequenceWithTraverse(Nil) shouldBe Some(List())
  }

  "traverse" should "traverse" in {
    SeqOperations.traverse(List(1,2,3,4))(a => if (a % 2 == 0) None else Some(a)) shouldBe None
    SeqOperations.traverse(List(1,2,3,4))(a => Some(a)) shouldBe Some(List(1,2,3,4))
  }

  "ANSWERtraverse" should "ANSWERtraverse" in {
    SeqOperations.traverse(List(1,2,3,4))(a => if (a % 2 == 0) None else Some(a)) shouldBe SeqOperations.ANSWERtraverse(List(1,2,3,4))(a => if (a % 2 == 0) None else Some(a))
    SeqOperations.traverse(List(1,2,3,4))(a => Some(a)) shouldBe SeqOperations.ANSWERtraverse(List(1,2,3,4))(a => Some(a))
  }
}
