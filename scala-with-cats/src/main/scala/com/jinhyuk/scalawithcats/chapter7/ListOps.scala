package com.jinhyuk.scalawithcats.chapter7

object ListOps {
  def map[A, B](as: List[A])(f: A => B): List[B] = as.foldRight(List.empty[B])((a, bs) => f(a) :: bs)
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as.foldRight(List.empty[B])((a, bs) => f(a) ++ bs)
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as.foldRight(List.empty[A])((a, bs) => if (f(a)) a :: bs else bs)
  def sum[A](nums: List[A])(implicit numeric: Numeric[A]): A = nums.foldRight(numeric.zero)(numeric.plus)
}
