package com.jinhyuk.scalawithcats.chapter5

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}

class AutobotSpec extends FlatSpec with Matchers with ScalaFutures {
  import Autobot._

  "getPowerLevel" should "return power level of a given autobot" in {
    whenReady(getPowerLevel("Jazz").value) { result =>
      result shouldBe Right(6)
    }
    whenReady(getPowerLevel("Bumblebee").value) { result =>
      result shouldBe Right(8)
    }
    whenReady(getPowerLevel("Hot Rod").value) { result =>
      result shouldBe Right(10)
    }
    whenReady(getPowerLevel("Captain").value) { result =>
      result shouldBe Left("Captain unreachable")
    }
  }

  "canSpecialMove" should "check whether two allies can make special move" in {
    whenReady(canSpecialMove("Jazz", "Bumblebee").value) { result =>
      result shouldBe Right(false)
    }
    whenReady(canSpecialMove("Jazz", "Hot Rod").value) { result =>
      result shouldBe Right(true)
    }
    whenReady(canSpecialMove("Jazz", "Captain").value) { result =>
      result shouldBe Left("Captain unreachable")
    }
  }

  "tacticalReport" should "return message saying whether two allies can make special move or not" in {
    tacticalReport("Jazz", "Bumblebee") shouldBe "Jazz and Bumblebee need a recharge."
    tacticalReport("Bumblebee", "Hot Rod") shouldBe "Bumblebee and Hot Rod are ready to roll out!"
    tacticalReport("Jazz", "Ironhide") shouldBe "Comms error: Ironhide unreachable"
  }
}
