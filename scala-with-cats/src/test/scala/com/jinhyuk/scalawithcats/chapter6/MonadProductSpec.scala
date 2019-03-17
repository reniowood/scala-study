package com.jinhyuk.scalawithcats.chapter6

import cats.instances.list._
import org.scalatest.{FlatSpec, Matchers}

class MonadProductSpec extends FlatSpec with Matchers {
  import MonadProduct._

  "product" should "return same as product of Semigroup" in {
    product(List(1, 2), List(3, 4)) shouldBe List((1, 3), (1, 4), (2, 3), (2, 4))
  }
}
