package com.jinhyuk.scalawithcats.chapter3

import org.scalatest.{FlatSpec, Matchers}

class PrintableSpec extends FlatSpec with Matchers {
  "Printable[Box]" should "format Box instances" in {
    import Printable._

    format(Box("hello world")) shouldBe "hello world"
    format(Box(true)) shouldBe "yes"
  }
}
