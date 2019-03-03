package fpinscala.state

import org.scalacheck.Properties
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.{FlatSpec, Matchers}

object StateSpec extends Properties("State") {
  import RNG._

  property("nonNegativeInt") = forAll { seed: Int =>
    nonNegativeInt(Simple(seed))._1 >= 0
  }

  property("double") = forAll { seed: Int =>
    val result = double(Simple(seed))._1

    result >= 0 && result < 1
  }

  property("ints") = forAll { (seed: Int, n: Int) =>
    (n >= 0 && n < 10000) ==> (ints(n)(Simple(seed))._1.length == n)
  }
}

class StateSpec extends FlatSpec with Matchers {
  import State._

  "simulateMachine" should "simulate simple candy dispenser correctly" in {
    simulateMachine(List(Turn, Turn, Turn, Turn)).run(Machine(locked = false, 10, 5)) shouldBe (14, 1)
  }
}