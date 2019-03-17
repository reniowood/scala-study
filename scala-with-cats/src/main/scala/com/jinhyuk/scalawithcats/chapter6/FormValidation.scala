package com.jinhyuk.scalawithcats.chapter6

import cats.data.Validated
import cats.syntax.apply._
import cats.syntax.either._
import cats.instances.list._

case class User(name: String, age: Int)

object FormValidation {
  type FormData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  def getValue(name: String)(formData: FormData): FailFast[String] =
    formData.get(name).toRight(List(s"$name field not specified"))

  def parseInt(name: String)(value: String): FailFast[Int] =
    Either.catchOnly[NumberFormatException](value.toInt).leftMap(_ => List(s"$name must be an integer string"))

  def nonBlank(name: String)(value: String): FailFast[String] =
    Right(value).ensure(List(s"$name cannot be blank"))(_.nonEmpty)

  def nonNegative(name: String)(value: Int): FailFast[Int] =
    Right(value).ensure(List(s"$name must be non-negative"))(_ >= 0)

  def readName(formData: FormData): FailFast[String] =
    getValue("name")(formData).flatMap(nonBlank("name"))

  def readAge(formData: FormData): FailFast[Int] =
    getValue("age")(formData).flatMap(parseInt("age")).flatMap(nonNegative("age"))

  def read(formData: FormData): FailSlow[User] =
    (
      Validated.fromEither(readName(formData)),
      Validated.fromEither(readAge(formData))
    ).mapN(User)
}
