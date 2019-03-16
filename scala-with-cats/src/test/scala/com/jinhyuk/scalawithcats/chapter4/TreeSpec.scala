package com.jinhyuk.scalawithcats.chapter4

import org.scalatest.{FlatSpec, Matchers}
import cats.syntax.functor._
import cats.syntax.flatMap._

class TreeSpec extends FlatSpec with Matchers {
  import Tree._

  "treeMonad" should "work on instances of Branch and Leaf" in {
    branch(leaf(3), leaf(4)).flatMap(x => branch(leaf(x - 1), leaf(x + 1)))
      .shouldBe(branch(branch(leaf(2), leaf(4)), branch(leaf(3), leaf(5))))
  }

  "treeMonad" should "provide Functor-like behavior for free" in {
    branch(leaf(3), leaf(4)).map(x => x + 1)
      .shouldBe(branch(leaf(4), leaf(5)))
  }

  "treeMonad" should "allow us to use for comprehensions" in {
    val tree = for {
      a <- branch(leaf(3), leaf(4))
      b <- branch(leaf(a - 1), leaf(a + 1))
    } yield b

    tree shouldBe branch(branch(leaf(2), leaf(4)), branch(leaf(3), leaf(5)))
  }
}
