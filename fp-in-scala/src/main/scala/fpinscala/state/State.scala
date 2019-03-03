package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (next, rng1) = rng.nextInt

    (if (next < 0) -(next + 1) else next, rng1)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (next, rng1) = nonNegativeInt(rng)

    (next / (Int.MaxValue.toDouble + 1), rng1)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)

    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng1) = double(rng)
    val (i, rng2) = rng1.nextInt

    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)

    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (List.empty, rng)
    else {
      val (value, rng1) = rng.nextInt
      val (values, rng2) = ints(count - 1)(rng1)

      (value :: values, rng2)
    }
  }

  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)

    (f(a, b), rng2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { rng =>
    fs.foldRight((List[A](), rng)) { (rand, result) =>
      val (valueNext, rngNext) = rand(result._2)

      (valueNext :: result._1, rngNext)
    }
  }

  def intsViaSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng1) = f(rng)

    g(a)(rng1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))
  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = this.flatMap(a => State(s => (f(a), s)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = this.flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, s1) = run(s)

    f(a).run(s1)
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}