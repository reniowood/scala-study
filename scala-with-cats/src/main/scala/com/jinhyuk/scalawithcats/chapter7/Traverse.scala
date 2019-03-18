package com.jinhyuk.scalawithcats.chapter7

import cats.Applicative
import cats.syntax.apply._
import cats.syntax.applicative._

object Traverse {
  import scala.language.higherKinds

  def listTraverse[F[_]: Applicative, A, B]
  (list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, B]
  (list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)
}
