package com.jinhyuk.scalawithcats.chapter2

import cats.syntax.semigroup._

object SuperAdder {
  def add[A](items: List[A])(implicit monoid: cats.Monoid[A]): A = items.foldRight(monoid.empty)(_ |+| _)

  case class Order(totalCost: Double, quantity: Double)

  implicit val orderAddMonoid: cats.Monoid[Order] = new cats.Monoid[Order] {
    override def empty: Order = Order(0, 0)
    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }
}
