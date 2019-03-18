package com.jinhyuk.scalawithcats.chapter7

import cats.data.Validated.{Invalid, Valid}
import org.scalatest.{FunSuite, Matchers}

class TraverseSpec extends FunSuite with Matchers {
  import Traverse._

  test("Traversing with Vectors") {
    import cats.instances.vector._ // for Applicative

    listSequence(List(Vector(1, 2), Vector(3, 4))) shouldBe Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
    listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))
      .shouldBe(Vector(List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6), List(2, 3, 5), List(2, 3, 6), List(2, 4, 5), List(2, 4, 6)))
  }

  test("Traversing with Options") {
    import cats.instances.option._ // for Applicative

    def process(inputs: List[Int]) =
      listTraverse(inputs)(n => if(n % 2 == 0) Some(n) else None)

    process(List(2, 4, 6)) shouldBe Some(List(2, 4, 6))
    process(List(1, 2, 3)) shouldBe None
  }

  test("Traversing with Validated") {
    import cats.data.Validated
    import cats.instances.list._ // for Monoid

    type ErrorsOr[A] = Validated[List[String], A]

    def process(inputs: List[Int]): ErrorsOr[List[Int]] =
      listTraverse(inputs) { n =>
        if(n % 2 == 0) {
          Validated.valid(n)
        } else {
          Validated.invalid(List(s"$n is not even"))
        }
      }

    process(List(2, 4, 6)) shouldBe Valid(List(2, 4, 6))
    process(List(1, 2, 3)) shouldBe Invalid(List("1 is not even", "3 is not even"))
  }
}
