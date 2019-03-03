package fpinscala.laziness

import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {
  import Stream._

  "toList" should "return list converted from a stream" in {
    Stream(1, 2, 3, 4).toList shouldBe List(1, 2, 3, 4)
    Stream().toList shouldBe List.empty
  }

  "take" should "return the first n elements of a stream" in {
    Stream(1, 2, 3, 4, 5).take(3).toList shouldBe Stream(1, 2, 3).toList
    Stream(1, 2, 3).take(5).toList shouldBe Stream(1, 2, 3).toList
    Stream().take(3).toList shouldBe Stream().toList
    Stream(1, 2, 3).take(0).toList shouldBe Stream().toList
  }

  "drop" should "return a stream skipping the first n elements" in {
    Stream(1, 2, 3, 4, 5).drop(3).toList shouldBe Stream(4, 5).toList
    Stream(1, 2, 3).drop(5).toList shouldBe Stream().toList
    Stream().drop(3).toList shouldBe Stream().toList
    Stream(1, 2, 3).drop(0).toList shouldBe Stream(1, 2, 3).toList
  }

  "takeWhile" should "return the all starting elements which satisfy match the given predicate" in {
    Stream(1, 2, 3, 4, 5).takeWhile(_ < 4).toList shouldBe Stream(1, 2, 3).toList
    Stream(1, 3, 5, 7, 9).takeWhile(_ % 2 == 0).toList shouldBe Stream().toList
    Stream(1, 2, 3, 4, 5).takeWhile(_ % 2 == 0).toList shouldBe Stream().toList
  }

  "forAll" should "return false if any of elements of a stream doesn't match the given predicate" in {
    Stream(1, 2, 3, 4, 5).forAll(_ < 6) shouldBe true
    Stream(1, 2, 3, 4, 5).forAll(_ < 3) shouldBe false
    Stream[Int]().forAll(_ > 5) shouldBe true
  }

  "takeWhileViaFoldRight" should "return same result as takeWhile for the given stream" in {
    Stream(1, 2, 3, 4, 5).takeWhileViaFoldRight(_ < 4).toList shouldBe Stream(1, 2, 3, 4, 5).takeWhile(_ < 4).toList
    Stream(1, 3, 5, 7, 9).takeWhileViaFoldRight(_ % 2 == 0).toList shouldBe Stream(1, 3, 5, 7, 9).takeWhile(_ % 2 == 0).toList
    Stream(1, 2, 3, 4, 5).takeWhileViaFoldRight(_ % 2 == 0).toList shouldBe Stream(1, 2, 3, 4, 5).takeWhile(_ % 2 == 0).toList
  }

  "headOption" should "return Some of head if a stream is not empty" in {
    Stream(1, 2, 3, 4).headOption shouldBe Some(1)
    Stream().headOption shouldBe None
  }

  "map" should "convert all elements of a stream with the given function" in {
    Stream(1, 2, 3, 4).map(_ + 5).toList shouldBe Stream(6, 7, 8, 9).toList
    Stream[Int]().map(_ + 5).toList shouldBe Stream().toList
  }

  "filter" should "remove elements don't match the given predicate" in {
    Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList shouldBe Stream(2, 4).toList
    Stream(1, 2, 3, 4).filter(_ < 5).toList shouldBe Stream(1, 2, 3, 4).toList
    Stream(1, 2, 3, 4).filter(_ > 5).toList shouldBe Stream().toList
    Stream[Int]().filter(_ % 2 == 0).toList shouldBe Stream().toList
  }

  "append" should "append the given stream to a stream" in {
    Stream(1, 2, 3, 4).append(Stream(5, 6, 7, 8)).toList shouldBe Stream(1, 2, 3, 4, 5, 6, 7, 8).toList
    Stream(1, 2, 3, 4).append(Stream()).toList shouldBe Stream(1, 2, 3, 4).toList
    Stream().append(Stream(1, 2, 3, 4)).toList shouldBe Stream(1, 2, 3, 4).toList
  }

  "flatMap" should "convert all elements of a stream with the given function generates stream" in {
    Stream(1, 2, 3, 4).flatMap(a => Stream(a + 1, a + 2)).toList shouldBe Stream(2, 3, 3, 4, 4, 5, 5, 6).toList
    Stream[Int]().flatMap(a => Stream(a, a + 2)).toList shouldBe Stream().toList
    Stream(1, 2, 3, 4).flatMap(_ => Stream()).toList shouldBe Stream().toList
  }

  "constant" should "generate infinite stream of the given integer" in {
    constant(3).take(5).toList shouldBe Stream(3, 3, 3, 3, 3).toList
    constant(3).take(10).toList shouldBe Stream(3, 3, 3, 3, 3, 3, 3, 3, 3, 3).toList
  }

  "from" should "generate infinite integer stream starts with the given integer" in {
    from(1).take(5).toList shouldBe Stream(1, 2, 3, 4, 5).toList
    from(5).take(5).toList shouldBe Stream(5, 6, 7, 8, 9).toList
  }

  "fibs" should "generate infinite stream of fibonacci sequence" in {
    fibs.take(10).toList shouldBe Stream(0, 1, 1, 2, 3, 5, 8, 13, 21, 34).toList
  }

  "MethodViaUnfold" should "generate infinite stream same as its original method generates" in {
    fibsViaUnfold.take(100).toList shouldBe fibs.take(100).toList
    fromViaUnfold(10).take(100).toList shouldBe from(10).take(100).toList
    constantViaUnfold(3).take(100).toList shouldBe constant(3).take(100).toList
    onesViaUnfold.take(100).toList shouldBe ones.take(100).toList
  }

  "mapViaUnfold" should "return same result as map" in {
    Stream(1, 2, 3, 4).mapViaUnfold(_ + 5).toList shouldBe Stream(1, 2, 3, 4).map(_ + 5).toList
  }

  "takeViaUnfold" should "return same result as take" in {
    Stream(1, 2, 3, 4, 5).takeViaUnfold(3).toList shouldBe Stream(1, 2, 3, 4, 5).take(3).toList
    Stream(1, 2, 3).takeViaUnfold(5).toList shouldBe Stream(1, 2, 3).take(5).toList
    Stream().takeViaUnfold(3).toList shouldBe Stream().take(3).toList
    Stream(1, 2, 3).takeViaUnfold(0).toList shouldBe Stream(1, 2, 3).take(0).toList
  }

  "takeWhileViaUnfold" should "return same result as takeWhile for the given stream" in {
    Stream(1, 2, 3, 4, 5).takeWhileViaUnfold(_ < 4).toList shouldBe Stream(1, 2, 3, 4, 5).takeWhile(_ < 4).toList
    Stream(1, 3, 5, 7, 9).takeWhileViaUnfold(_ % 2 == 0).toList shouldBe Stream(1, 3, 5, 7, 9).takeWhile(_ % 2 == 0).toList
    Stream(1, 2, 3, 4, 5).takeWhileViaUnfold(_ % 2 == 0).toList shouldBe Stream(1, 2, 3, 4, 5).takeWhile(_ % 2 == 0).toList
  }

  "zipWith" should "return stream zipped with the given two streams" in {
    Stream(1, 2, 3, 4, 5).zipWith(Stream(5, 4, 3, 2, 1))(_ + _).toList shouldBe Stream(6, 6, 6, 6, 6).toList
    Stream(1, 2, 3, 4, 5).zipWith(Stream(5, 4, 3))(_ + _).toList shouldBe Stream(6, 6, 6).toList
    Stream(1, 2, 3).zipWith(Stream(5, 4, 3, 2, 1))(_ + _).toList shouldBe Stream(6, 6, 6).toList
    Stream[Int]().zipWith(Stream(5, 4, 3, 2, 1))(_ + _).toList shouldBe Stream().toList
    Stream(1, 2, 3).zipWith(Stream[Int]())(_ + _).toList shouldBe Stream().toList
  }

  "zipAll" should "continue the traversal as long as either stream has more elements" in {
    Stream(1, 2, 3, 4, 5).zipAll(Stream(5, 4, 3, 2, 1)).toList shouldBe Stream((Some(1), Some(5)), (Some(2), Some(4)), (Some(3), Some(3)), (Some(4), Some(2)), (Some(5), Some(1))).toList
    Stream(1, 2, 3, 4, 5).zipAll(Stream(5, 4, 3)).toList shouldBe Stream((Some(1), Some(5)), (Some(2), Some(4)), (Some(3), Some(3)), (Some(4), None), (Some(5), None)).toList
    Stream(1, 2, 3).zipAll(Stream(5, 4, 3, 2, 1)).toList shouldBe Stream((Some(1), Some(5)), (Some(2), Some(4)), (Some(3), Some(3)), (None, Some(2)), (None, Some(1))).toList
    Stream[Int]().zipAll(Stream(5, 4, 3, 2, 1)).toList shouldBe Stream((None, Some(5)), (None, Some(4)), (None, Some(3)), (None, Some(2)), (None, Some(1))).toList
    Stream(1, 2, 3).zipAll(Stream[Int]()).toList shouldBe Stream((Some(1), None), (Some(2), None), (Some(3), None)).toList
  }

  "startsWith" should "check whether a stream starts with the given stream" in {
    Stream(1, 2, 3, 4).startsWith(Stream(1, 2)) shouldBe true
    Stream(1, 2, 3, 4).startsWith(Stream(1, 2, 3, 4)) shouldBe true
    Stream(1, 2, 3).startsWith(Stream(1, 2, 3, 4)) shouldBe false
    Stream(1, 2, 3).startsWith(Stream()) shouldBe true
  }

  "tails" should "return the stream of suffixes of the input stream" in {
    Stream(1, 2, 3).tails.toList.map(_.toList) shouldBe List(List(1, 2, 3), List(2, 3), List(3), List())
    Stream[Int]().tails.toList.map(_.toList) shouldBe List(List())
  }

  "scanRight" should "be like a foldRight that returns a stream of the intermediate results" in {
    Stream(1, 2, 3).scanRight(0)(_ + _).toList shouldBe List(6, 5, 3, 0)
  }
}
