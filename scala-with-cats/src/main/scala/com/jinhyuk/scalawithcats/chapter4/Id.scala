package com.jinhyuk.scalawithcats.chapter4

object Id {
  import cats.Id

  def pure[A](value: A): Id[A] = value
  def flatMap[A, B](value: Id[A])(f: A => Id[B]): Id[B] = f(value)
  def map[A, B](value: Id[A])(f: A => B): Unit = pure(f(value))
}
