package com.jinhyuk.scalawithcats.chapter6

import cats.data.Validated.{Invalid, Valid}
import org.scalatest.{FlatSpec, Matchers}

class FormValidationSpec extends FlatSpec with Matchers {
  import FormValidation._

  "getValue" should "read value from the FormData given a field name" in {
    getValue("name")(Map("name" -> "Jinhyuk", "age" -> "28")) shouldBe Right("Jinhyuk")
    getValue("b")(Map("name" -> "Jinhyuk", "age" -> "28")) shouldBe Left(List("b field not specified"))
  }

  "parseInt" should "parse a given integer string" in {
    parseInt("age")("123") shouldBe Right(123)
    parseInt("age")("a123") shouldBe Left(List("age must be an integer string"))
  }

  "nonBlank" should "check whether a given string is blank or not" in {
    nonBlank("name")("rylan.k") shouldBe Right("rylan.k")
    nonBlank("name")("") shouldBe Left(List("name cannot be blank"))
  }

  "nonNegative" should "check whether a given integer is negative nor not" in {
    nonNegative("age")(10) shouldBe Right(10)
    nonNegative("age")(-1) shouldBe Left(List("age must be non-negative"))
  }

  "readName" should "read name from a given FormData" in {
    readName(Map("name" -> "Dade Murphy")) shouldBe Right("Dade Murphy")
    readName(Map("name" -> "")) shouldBe Left(List("name cannot be blank"))
    readName(Map()) shouldBe Left(List("name field not specified"))
  }

  "readAge" should "read age from a given FormData" in {
    readAge(Map("age" -> "11")) shouldBe Right(11)
    readAge(Map("age" -> "-1")) shouldBe Left(List("age must be non-negative"))
    readAge(Map()) shouldBe Left(List("age field not specified"))
  }

  "read" should "read FormData and produce an User object" in {
    read(Map("name" -> "John Doe", "age" -> "32")) shouldBe Valid(User(name = "John Doe", age = 32))
    read(Map("name" -> "John Doe", "age" -> "thirty-two")) shouldBe Invalid(List("age must be an integer string"))
    read(Map("name" -> "", "age" -> "32")) shouldBe Invalid(List("name cannot be blank"))
    read(Map("name" -> "", "age" -> "-5")) shouldBe Invalid(List("name cannot be blank", "age must be non-negative"))
  }
}
