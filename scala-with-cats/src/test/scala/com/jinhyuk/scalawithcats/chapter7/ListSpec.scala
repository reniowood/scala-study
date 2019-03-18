package com.jinhyuk.scalawithcats.chapter7

import org.scalatest.{FlatSpec, Matchers}

class ListSpec extends FlatSpec with Matchers {
  import ListOps._

  "foldRight with an empty list and ::" should "return the original list" in {
    List(1, 2, 3).foldRight(List.empty[Int])((i, a) => i :: a) shouldBe List(1, 2, 3)
  }

  "foldLeft with an empty list and ::" should "return reversed list" in {
    List(1, 2, 3).foldLeft(List.empty[Int])((a, i) => i :: a) shouldBe List(3, 2, 1)
  }

  "map in terms of foldRight" should "return a result same as the original one" in {
    map(List(1, 2, 3))(_ + 1) shouldBe List(1, 2, 3).map(_ + 1)
  }

  "flatMap in terms of foldRight" should "return a result same as the original one" in {
    flatMap(List(1, 2, 3))(x => List(x + 1, x + 2)) shouldBe List(1, 2, 3).flatMap(x => List(x + 1, x + 2))
  }

  "filter in terms of foldRight" should "return a result same as the original one" in {
    filter(List(1, 2, 3))(_ % 2 == 0) shouldBe List(1, 2, 3).filter(_ % 2 == 0)
  }

  "sum in terms of foldRight" should "return a result same as the original one" in {
    sum(List(1, 2, 3)) shouldBe List(1, 2, 3).sum
    sum(List(1.0, 2.5, 3.7)) shouldBe List(1.0, 2.5, 3.7).sum
  }
}
