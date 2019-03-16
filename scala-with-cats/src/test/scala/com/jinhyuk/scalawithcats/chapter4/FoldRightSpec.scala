package com.jinhyuk.scalawithcats.chapter4

import org.scalatest.{FlatSpec, Matchers}

class FoldRightSpec extends FlatSpec with Matchers {
  import FoldRight._

  "foldRight" should "fail to be executed with very long list" in {
    assertThrows[StackOverflowError] {
      foldRight(List.fill(10000)(1), 0)(_ + _)
    }
  }

  "foldRightViaEval" should "be executed with very long list" in {
    foldRightViaEval((1 to 100000).toList, 0L)(_ + _) shouldBe 5000050000L
  }
}
