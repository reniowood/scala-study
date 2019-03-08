package com.jinhyuk.scalawithcats.chapter2

object SetSemigroup {
  implicit def setIntersectionSemigroup[A]: Semigroup[Set[A]] = new Semigroup[Set[A]] {
    override def combine(x: Set[A], y: Set[A]): Set[A] = x intersect y
  }
}
