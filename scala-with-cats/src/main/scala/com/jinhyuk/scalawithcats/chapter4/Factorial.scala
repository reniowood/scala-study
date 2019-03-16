package com.jinhyuk.scalawithcats.chapter4

import cats.data.Writer
import cats.syntax.applicative._
import cats.syntax.writer._
import cats.instances.vector._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Factorial {
  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if(n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  type Logged[A] = Writer[Vector[String], A]

  def factorialWithWriter(n: Int): Logged[Int] =
    for {
      ans <- if (n == 0) 1.pure[Logged] else slowly(factorialWithWriter(n - 1).map(_ * n))
      _ <- Vector(s"fact $n $ans").tell
    } yield ans

  def main(args: Array[String]): Unit = {
    // factorial logs without Writer
    Await.result(Future.sequence(Vector(
      Future(factorial(3)),
      Future(factorial(3))
    )), 5.seconds)

    // factorial logs with Writer
    val Vector((logA, _), (logB, _)) = Await.result(Future.sequence(Vector(
      Future(factorialWithWriter(3).run),
      Future(factorialWithWriter(5).run)
    )), 5.seconds)

    println(logA)
    println(logB)
  }
}
