package com.jinhyuk.scalawithcats.chapter3

import org.scalatest.{FlatSpec, Matchers}

class CodecSpec extends FlatSpec with Matchers {
  "Codec" should "convert instance to String and String to instance" in {
    import Codec._

    encode(3) shouldBe "3"
    decode[Int]("3") shouldBe 3

    encode(true) shouldBe "true"
    decode[Boolean]("true") shouldBe true

    encode(123.4) shouldBe "123.4"
    decode[Double]("123.4") shouldBe 123.4

    encode(Box(123.4)) shouldBe "123.4"
    decode[Box[Double]]("123.4") shouldBe Box(123.4)
  }
}
