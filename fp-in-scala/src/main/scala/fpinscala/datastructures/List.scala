package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new RuntimeException("Empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (_, 0) => l
    case (Nil, _) => Nil
    case (Cons(_, t), m) => drop(t, m - 1)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new RuntimeException("Empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, len) => len + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)((s, h) => s + h)

  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)((p, h) => p * h)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((r, h) => Cons(h, r))

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(l), z)((a, b) => f(b, a))

  def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldLeft3[A,B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldRight3[A,B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight2(a1, a2)((h, l) => Cons(h, l))

  def flatten[A](l: List[List[A]]): List[A] = foldRight(l, Nil:List[A])(append)

  def add1(ns: List[Int]): List[Int] = foldRight(ns, Nil:List[Int])((x, acc) => Cons(x + 1, acc))

  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil:List[String])((x, acc) => Cons(x.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((x, acc) => Cons(f(x), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil:List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  def zipIntList(ns1: List[Int], ns2: List[Int]): List[Int] = (ns1, ns2) match {
    case (Nil, _) | (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipIntList(t1, t2))
  }

  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _) | (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def startsWith[A](l1: List[A], l2: List[A]): Boolean = (l1, l2) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(h1, t1), Cons(h2, t2)) => h1 == h2 && startsWith(t1, t2)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case Cons(_, t) => startsWith(sup, sub) || hasSubsequence(t, sub)
  }
}