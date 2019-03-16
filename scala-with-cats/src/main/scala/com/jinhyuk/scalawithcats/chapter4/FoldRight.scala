package com.jinhyuk.scalawithcats.chapter4

import cats.Eval

object FoldRight {
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, foldRight(tail, acc)(fn))
      case Nil =>
        acc
    }

  def foldRightViaEval[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {
    def go(as: List[A], acc: B): Eval[B] =
      as match {
        case head :: tail =>
          Eval.defer(go(tail, acc)).map(fn(head, _))
        case Nil =>
          Eval.now(acc)
      }

    go(as, acc).value
  }
}
