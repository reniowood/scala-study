package fpinscala.errorhandling

import org.scalatest.{FlatSpec, Matchers}

class EitherSpec extends FlatSpec with Matchers {
  import Either._

  "map" should "convert value of Right" in {
    Right(3).map(_ + 1) shouldBe Right(4)
    (Left("failed"):Either[String, Int]).map(_ + 5) shouldBe Left("failed")
  }

  "flatMap" should "new Either from given function" in {
    Right(3).flatMap(x => Right(x + 2)) shouldBe Right(5)
    Right(5).flatMap(x => Left(x - 2)) shouldBe Left(3)
    (Left(3):Either[Int, Int]).flatMap(x => Right(x + 4)) shouldBe Left(3)
  }

  "orElse" should "return caller if it is Right or default value" in {
    Right(4).orElse(Right(3)) shouldBe Right(4)
    (Left("failed"):Either[String, Int]).orElse(Right(3)) shouldBe Right(3)
    Right(4).orElse(Left("failed"):Either[String, Int]) shouldBe Right(4)
    (Left("failed"):Either[String, Int]).orElse(Left("bad")) shouldBe Left("bad")
  }

  "map2" should "combine two Either values by given binary function" in {
    Right(4).map2(Right(5))(_ + _) shouldBe Right(9)
    Right(4).map2(Left("failed"):Either[String, Int])(_ + _) shouldBe Left("failed")
    (Left("failed"):Either[String, Int]).map2(Right(5))(_ + _) shouldBe Left("failed")
    (Left("failed"):Either[String, Int]).map2(Left("bad"))(_ + _) shouldBe Left("failed")
  }

  "sequence" should "return Either of list converted from list of Either" in {
    sequence(List(Right(4), Right(5), Right(2))) shouldBe Right(List(4, 5, 2))
    sequence(List(Right(4), Right(5), Left("failed"), Right(2))) shouldBe Left("failed")
    sequence(List(Left("failed"), Right(4), Right(5), Right(2))) shouldBe Left("failed")
    sequence(List(Right(4), Right(5), Right(2), Left("failed"))) shouldBe Left("failed")
    sequence(List(Left("failed1"), Left("failed2"))) shouldBe Left("failed1")
    sequence(List.empty) shouldBe Right(List.empty)
  }

  "traverse" should "return option of list applied given function" in {
    traverse(List("1", "2", "3"))(x => Try(x.toInt)) shouldBe Right(List(1, 2, 3))
    traverse(List("1", "2", "aa"))(x => Try(x.toInt)) should matchPattern { case Left(x) if x.isInstanceOf[NumberFormatException] => }
  }
}
