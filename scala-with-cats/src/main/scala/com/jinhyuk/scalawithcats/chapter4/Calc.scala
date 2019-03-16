package com.jinhyuk.scalawithcats.chapter4

import cats.data.State
import cats.instances.int._

object Calc {
  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    State[List[Int], Int] { oldStack =>
      val opMap: Map[String, (Int, Int) => Int] =
        Map("+" -> (_ + _), "-" -> (_ - _), "*" -> (_ * _), "/" -> (_ / _))

      sym match {
        case num if num forall Character.isDigit => (num.toInt :: oldStack, num.toInt)
        case op =>
          val List(second, first) = oldStack.take(2)
          val result = opMap(op)(first, second)

          (result :: oldStack.drop(2), result)
      }
    }

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(State.empty[List[Int], Int])((state, symbol) => state.flatMap(_ => evalOne(symbol)))

  def evalInput(input: String): Int =
    evalAll(input.split(' ').toList).runA(Nil).value
}
