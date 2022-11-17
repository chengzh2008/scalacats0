package sandbox.cats4

import cats.Eval

object SafeFoldingEval {
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, foldRight(tail, acc)(fn))
      case Nil =>
        acc
    }

  // Eval for the safe stack
  def foldRight1[A, B](as: Eval[List[A]], acc: Eval[B])(
      fn: (A, B) => Eval[B]
  ): Eval[B] =
    as.value match {
      case head :: tail =>
        fn(head, Eval.defer(foldRight1(Eval.now(tail), acc)(fn)).value)
      case Nil =>
        acc
    }

  def foldRightRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {
    def foldRightEval(as: List[A], acc: Eval[B])(
        fn: (A, Eval[B]) => Eval[B]
    ): Eval[B] =
      as match {
        case head :: tail =>
          Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
        case Nil => acc
      }

    // wrong implementation though type checking
    // foldRightEval(as, Eval.now(acc))((a, eb) => Eval.now(fn(a, eb.value))).value
    // 5000050000
    foldRightEval(as, Eval.now(acc))((a, eb) => eb.map(fn(a, _))).value
  }
}

object SafeFoldingEvalTest extends App {
  // blow up :-(
  // println(SafeFoldingEval.foldRight((1 to 100000).toList, 0L)(_ + _))
  println(SafeFoldingEval.foldRightRight((1 to 100000).toList, 0L)(_ + _))
}
