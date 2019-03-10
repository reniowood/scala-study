package com.jinhyuk.scalawithcats.chapter3

import cats.Functor
import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {
  "Functor[Tree]" should "transform value of Tree" in {
    import Tree._

    Functor[Tree].map(Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))(_ + 5)
      .shouldBe(Branch(Branch(Leaf(8), Leaf(9)), Leaf(10)))
  }
}
