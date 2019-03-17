package com.jinhyuk.scalawithcats.chapter5

import cats.data.EitherT
import cats.instances.future._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object Autobot {
  import scala.concurrent.ExecutionContext.Implicits.global

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] =
    EitherT.fromOptionF(Future(powerLevels.get(autobot)), ifNone = s"$autobot unreachable")

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      level1 <- getPowerLevel(ally1)
      level2 <- getPowerLevel(ally2)
    } yield level1 + level2 > 15

  def tacticalReport(ally1: String, ally2: String): String =
    Await.result(canSpecialMove(ally1, ally2).value.map {
      case Left(message) => s"Comms error: $message"
      case Right(false) => s"$ally1 and $ally2 need a recharge."
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
    }, 1.second)
}
