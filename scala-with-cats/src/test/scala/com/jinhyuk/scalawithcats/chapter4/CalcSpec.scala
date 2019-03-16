package com.jinhyuk.scalawithcats.chapter4

import org.scalatest.{FlatSpec, Matchers}

class CalcSpec extends FlatSpec with Matchers {
  import Calc._

  "evalOne" should "eval a given symbol" in {
    evalOne("42").runA(Nil).value shouldBe 42

    val program = for {
      _   <- evalOne("1")
      _   <- evalOne("2")
      ans <- evalOne("+")
    } yield ans
    program.runA(Nil).value shouldBe 3
  }

  "evalAll" should "eval a given list of symbol" in {
    val program = evalAll(List("1", "2", "+", "3", "*"))
    program.runA(Nil).value shouldBe 9
  }

  "evalAll and evalOne" should "be used with each other" in {
    val program = for {
      _   <- evalAll(List("1", "2", "+"))
      _   <- evalAll(List("3", "4", "+"))
      ans <- evalOne("*")
    } yield ans

    program.runA(Nil).value shouldBe 21
  }

  "evalInput" should "evaluate input and return the result" in {
    evalInput("1 2 + 3 4 + *") shouldBe 21
  }
}
