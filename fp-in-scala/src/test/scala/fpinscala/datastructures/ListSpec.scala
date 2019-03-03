package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class ListSpec extends FlatSpec with Matchers {
  import List._
  
  "Exercise 3.1" should "return value 3" in {
    x shouldBe 3
  }

  "tail" should "return tail of list" in {
    tail(List(1, 2, 3, 4)) shouldBe List(2, 3, 4)
    tail(List(1)) shouldBe Nil
    assertThrows[RuntimeException] {
      tail(Nil)
    }
  }

  "setHead" should "replace head of the list" in {
    setHead(List(1, 2, 3, 4), 3) shouldBe List(3, 2, 3, 4)
    setHead(List(1), 2) shouldBe List(2)
    assertThrows[RuntimeException] {
      setHead(Nil, 2)
    }
  }

  "drop" should "drop first n elements of the list" in {
    drop(List(1, 2, 3, 4), 2) shouldBe List(3, 4)
    drop(List(1, 2, 3, 4), 0) shouldBe List(1, 2, 3, 4)
    drop(List(1, 2, 3, 4), 5) shouldBe Nil
    drop(Nil, 3) shouldBe Nil
  }

  "dropWhile" should "drop a prefix of the list satisfies predicate" in {
    dropWhile(List(1, 3, 5, 2, 4, 6))(_ % 2 == 1) shouldBe List(2, 4, 6)
    dropWhile(List(1, 2, 3, 4, 5, 6))(_ % 2 == 1) shouldBe List(2, 3, 4, 5, 6)
    dropWhile(List(1, 2, 3, 4))(_ > 5) shouldBe List(1, 2, 3, 4)
    dropWhile(Nil)(_ => true) shouldBe Nil
  }

  "init" should "return the list except the last element" in {
    init(List(1, 2, 3, 4)) shouldBe List(1, 2, 3)
    init(List(3)) shouldBe Nil
    assertThrows[RuntimeException] {
      init(Nil)
    }
  }
  
  // Exercise 3.7
  /*
    No. There is no short-circuiting logic in the foldRight method.
   */
  
  "constructor of List" should "be implemented with foldRight" in {
    Cons(1, Cons(2, Cons(3, Nil))) shouldBe foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _))
  }

  "length" should "return length of the list" in {
    List.length(List(1, 2, 3)) shouldBe 3
    List.length(Nil) shouldBe 0
  }

  def fill[A](n: Int, value: A): List[A] = {
    @tailrec
    def go(n: Int, l: List[A]): List[A] = n match {
      case 0 => l
      case m => go(m - 1, Cons(value, l))
    }

    go(n, Nil)
  }

  "calling foldRight with very long list" should "throw StackOverflowError" in {
    assertThrows[StackOverflowError] {
      List.length(fill(10000, 3))
    }
  }

  "calling foldLeft with very long list" should "work well" in {
    foldLeft(fill(10000, 1), 0)((c, _) => c + 1) shouldBe 10000
  }

  "calling sum3 with very long list" should "work well" in {
    sum3(fill(10000, 1)) shouldBe 10000
  }

  "calling product3 with very long list" should "work well" in {
    product3(fill(10000, 1.0)) shouldBe 1.0
  }

  "reverse" should "reverse the list" in {
    reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
    reverse(Nil) shouldBe Nil
  }

  "foldLeft2" should "work well" in {
    foldLeft2(List(1, 2, 3, 4), Nil:List[Int])((b, a) => Cons(a, b)) shouldBe List(4, 3, 2, 1)
//    foldLeft2(fill(10000, 1), 0)((c, _) => c + 1) shouldBe 10000
  }

  "foldRight2" should "work well" in {
    foldRight2(List(1, 2, 3, 4), Nil:List[Int])(Cons(_, _)) shouldBe List(1, 2, 3, 4)
    foldRight2(fill(10000, 1), 0)((_, c) => c + 1) shouldBe 10000
  }

  "foldLeft3" should "work well" in {
    foldLeft3(List(1, 2, 3, 4), Nil:List[Int])((b, a) => Cons(a, b)) shouldBe List(4, 3, 2, 1)
  }

  "foldRight3" should "work well" in {
    foldRight3(List(1, 2, 3, 4), Nil:List[Int])(Cons(_, _)) shouldBe List(1, 2, 3, 4)
  }

  "append2" should "work same as append" in {
    append2(List(1, 2, 3), List(4, 5, 6)) shouldBe append(List(1, 2, 3), List(4, 5, 6))
    append2(Nil, List(4, 5, 6)) shouldBe append(Nil, List(4, 5, 6))
    append2(List(1, 2, 3), Nil) shouldBe append(List(1, 2, 3), Nil)
    append2(fill(1000, 1), fill(1000, 1)) shouldBe fill(2000, 1)
  }

  "flatten" should "flatten list of list" in {
    flatten(List(List(1, 2, 3), List(4), List(5))) shouldBe List(1, 2, 3, 4, 5)
    flatten(Nil) shouldBe Nil
    flatten(List(Nil, List(3, 4))) shouldBe List(3, 4)
    flatten(List(List(1, 2), Nil, List(3, 4, 5))) shouldBe List(1, 2, 3, 4, 5)
    flatten(List(List(1, 2), List(3), Nil)) shouldBe List(1, 2, 3)
  }

  "add1" should "add 1 to each element of the input list" in {
    add1(List(1, 2, 3)) shouldBe List(2, 3, 4)
    add1(Nil) shouldBe Nil
  }

  "doubleToString" should "convert each double element to string" in {
    doubleToString(List(1.0, 3.5, 4.8)) shouldBe List("1.0", "3.5", "4.8")
    doubleToString(Nil) shouldBe Nil
  }

  "map" should "apply function to each element of the input list" in {
    map(List(1, 2, 3))(_ * 2) shouldBe List(2, 4, 6)
    map(List("abc", "bac", "cba"))(_.charAt(0)) shouldBe List('a', 'b', 'c')
    map(Nil:List[Int])(_ + 2) shouldBe Nil
  }

  "filter" should "filter elements doesn't satisfy given condition" in {
    filter(List(1, 2, 3)) (_ % 2 == 0) shouldBe List(2)
    filter(List(1, 2, 3, 4, 5))(_ < 3) shouldBe List(1, 2)
    filter(List(2, 5, 10, 12, 4))(_ > 20) shouldBe Nil
    filter(Nil:List[Int])(_ > 3) shouldBe Nil
  }

  "flatMap" should "map element to list of element" in {
    flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
    flatMap(List(3, 4, 5))(i => List(i, i + 1)) shouldBe List(3, 4, 4, 5, 5, 6)
    flatMap(Nil:List[Int])(i => List(i, i)) shouldBe Nil
  }

  "filter2" should "work same as filter" in {
    filter2(List(1, 2, 3)) (_ % 2 == 0) shouldBe filter(List(1, 2, 3)) (_ % 2 == 0)
    filter2(List(1, 2, 3, 4, 5))(_ < 3) shouldBe filter(List(1, 2, 3, 4, 5))(_ < 3)
    filter2(List(2, 5, 10, 12, 4))(_ > 20) shouldBe filter(List(2, 5, 10, 12, 4))(_ > 20)
    filter2(Nil:List[Int])(_ > 3) shouldBe filter(Nil:List[Int])(_ > 3)
  }

  "zipIntList" should "zip two integer lists" in {
    zipIntList(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
    zipIntList(List(1, 2, 3), List(3, 5)) shouldBe List(4, 7)
    zipIntList(List(1, 2), List(4, 5, 6)) shouldBe List(5, 7)
    zipIntList(List(1, 2, 3), Nil) shouldBe Nil
    zipIntList(Nil, List(4, 5, 6)) shouldBe Nil
    zipIntList(Nil, Nil) shouldBe Nil
  }

  "zipWith" should "zip elements of two lists and apply function" in {
    zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) shouldBe List(5, 7, 9)
    zipWith(List("a", "b", "c"), List("d", "e", "f"))(_ + _) shouldBe List("ad", "be", "cf")
    zipWith(List(1, 2, 3), List(4, 5))(_ + _) shouldBe List(5, 7)
    zipWith(List(1, 2), List(4, 5, 6))(_ + _) shouldBe List(5, 7)
    zipWith(List(1, 2, 3), Nil)(_ + _) shouldBe Nil
    zipWith(Nil:List[Int], List(4, 5, 6))(_ + _) shouldBe Nil
    zipWith(Nil:List[Int], Nil:List[Int])(_ + _) shouldBe Nil
  }

  "hasSubsequence" should "find given list is a subsequence of the list" in {
    hasSubsequence(List(1, 2, 3, 4), List(1, 2)) shouldBe true
    hasSubsequence(List(1, 2, 3, 4), List(2, 3)) shouldBe true
    hasSubsequence(List(1, 2, 3, 4), List(4)) shouldBe true
    hasSubsequence(List(1, 2, 3, 4), List(1, 3, 4)) shouldBe false
    hasSubsequence(Nil, Nil) shouldBe true
    hasSubsequence(Nil, List(1)) shouldBe false
  }
}
