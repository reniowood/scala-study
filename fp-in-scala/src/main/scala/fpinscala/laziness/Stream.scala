package fpinscala.laziness

import fpinscala.laziness.Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => List.empty
  }

  def take(n: Int): Stream[A] =
    if (n == 0) Empty else this match {
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
      case Empty => Empty
    }

  def drop(n: Int): Stream[A] =
    if (n == 0) this else this match {
      case Cons(_, t) => t().drop(n - 1)
      case Empty => Empty
    }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (p(a)) cons(a, b) else Empty)

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => p(h()) && t().forAll(p)
  }

  def headOption: Option[A] = foldRight(None:Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(Empty:Stream[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty:Stream[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] = this match {
    case Cons(h, t) => Cons(h, () => t().append(s))
    case Empty => s
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty:Stream[B])((a, b) => f(a).append(b))

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), x) if x > 0 => Some((h(), (t(), x - 1)))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
    case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
    case _ => None
  }

  def startsWith[B](s: Stream[B]): Boolean = zipAll(s).takeWhile(_._2.isDefined).forAll {
    case (h, h2) => h == h2
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Cons(h, t) => Some((Cons(h, t), t()))
    case Empty => None
  } append Stream(Empty)

//  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] = this match {
//    case Cons(h, t) =>
//      val b = t().scanRight(z)(f)
//      Cons(() => f(h(), b.headOption.getOrElse(z)), () => b)
//    case Empty => Stream(z)
//  }

  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p) => {
      lazy val p1 = p
      val b = f(a, p1._1)
      (b, cons(b, p1._2))
    })._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = Stream.cons(a, go(b, a + b))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((v, s)) => cons(v, unfold(s)(f))
    case None => Empty
  }

  def fibsViaUnfold: Stream[Int] = unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(a => Some((a, a)))

  def onesViaUnfold: Stream[Int] = unfold(1)(_ => Some((1, 1)))
}