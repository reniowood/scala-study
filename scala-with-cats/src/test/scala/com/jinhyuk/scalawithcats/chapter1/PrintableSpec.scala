package com.jinhyuk.scalawithcats.chapter1

import org.scalatest.{FunSpec, Matchers}

class PrintableSpec extends FunSpec with Matchers {
  describe("introduction.Printable") {
    it("should format integer and string value") {
      import PrintableInstances._

      Printable.format(3) shouldBe "3"
      Printable.format("3") shouldBe "3"
    }
  }
}
