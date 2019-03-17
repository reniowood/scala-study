package com.jinhyuk.scalawithcats.chapter6

import cats.syntax.flatMap._ // for flatMap
import cats.syntax.functor._ // for map

object MonadProduct {
  import cats.Monad

  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    for {
      a <- x
      b <- y
    } yield (a, b)
}
