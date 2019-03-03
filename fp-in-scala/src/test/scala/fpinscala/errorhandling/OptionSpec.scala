package fpinscala.errorhandling

import org.scalatest.{FlatSpec, Matchers}

class OptionSpec extends FlatSpec with Matchers {
  import Option._

  "map" should "convert value of Some" in {
    Some(3).map(_ + 1) shouldBe Some(4)
    (None:Option[Int]).map(_ + 5) shouldBe None
  }

  "getOrElse" should "return value of Some or default value if Option is None" in {
    Some(4).getOrElse(5) shouldBe 4
    (None:Option[String]).getOrElse("hello") shouldBe  "hello"
  }

  "flatMap" should "return new Option from given function" in {
    Some(4).flatMap(x => Some(x + 2)) shouldBe Some(6)
    Some(4).flatMap(_ => None) shouldBe None
    (None:Option[Int]).flatMap(x => Some(x + 2)) shouldBe None
  }

  "orElse" should "return caller if it is Some or default value" in {
    Some(4).orElse(Some(3)) shouldBe Some(4)
    (None:Option[String]).orElse(Some("hello")) shouldBe Some("hello")
    Some(4).orElse(None) shouldBe Some(4)
    (None:Option[String]).orElse(None) shouldBe None
  }

  "filter" should "return None if value of Some is not satisfied by given function" in {
    Some(5).filter(_ % 2 == 0) shouldBe None
    Some(4).filter(_ % 2 == 0) shouldBe Some(4)
    (None:Option[Int]).filter(_ % 2 == 0) shouldBe None
    (None:Option[Int]).filter(_ => true) shouldBe None
  }

  "map2" should "combine two Option values by given binary function" in {
    map2(Some(4), Some(5))(_ + _) shouldBe Some(9)
    map2(Some(4), None:Option[Int])(_ + _) shouldBe None
    map2(None:Option[Int], Some(5))(_ + _) shouldBe None
    map2(None:Option[Int], None:Option[Int])(_ + _) shouldBe None
  }

  "sequence" should "return option of list converted from list of option" in {
    sequence(List(Some(4), Some(5), Some(2))) shouldBe Some(List(4, 5, 2))
    sequence(List(Some(4), Some(5), None, Some(2))) shouldBe None
    sequence(List(None, Some(4), Some(5), Some(2))) shouldBe None
    sequence(List(Some(4), Some(5), Some(2), None)) shouldBe None
    sequence(List(None, None)) shouldBe None
    sequence(List.empty) shouldBe Some(List.empty)
  }

  "traverse" should "return option of list applied given function" in {
    traverse(List("1", "2", "3"))(x => Try(x.toInt)) shouldBe Some(List(1, 2, 3))
    traverse(List("1", "2", "aa"))(x => Try(x.toInt)) shouldBe None
  }
}
