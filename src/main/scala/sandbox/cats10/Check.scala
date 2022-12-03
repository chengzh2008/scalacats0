package sandbox.cats10

import cats.Semigroup
import cats.data.Validated

sealed trait Check[E, A, B] {
  import Check._
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](f: B => C): Check[E, A, C] = Map(this, f)

  def flatmap[C](f: B => Check[E, A, C]) = FlatMap(this, f)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] = AndThen(this, that)
}

object Check {
  final case class Map[E, A, B, C](check: Check[E, A, B], f: B => C)
      extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).map(f)
  }

  final case class FlatMap[E, A, B, C](
      check: Check[E, A, B],
      f: B => Check[E, A, C]
  ) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      // check(a) match {
      //   case Valid(b)   => f(b)(a)
      //   case Invalid(e) => Invalid(e)
      // }
      // or levarage either monadic properties
      check(a).withEither(_.flatMap(b => f(b)(a).toEither))
  }

  final case class AndThen[E, A, B, C](
      check: Check[E, A, B],
      that: Check[E, B, C]
  ) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      // check(a) match {
      //   case Valid(b)   => that(b)
      //   case Invalid(e) => Invalid(e)
      // }
      // or levarage either monadic properties
      check(a).withEither(_.flatMap(b => that(b).toEither))

  }

  final case class Pure[E, A, B](f: A => Validated[E, B])
      extends Check[E, A, B] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B] =
      f(a)
  }

  final case class PurePredicate[E, A](
      pred: Predicate[E, A]
  ) extends Check[E, A, A] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      pred(a)
  }

  def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
    PurePredicate(pred)

  def apply[E, A, B](f: A => Validated[E, B]): Check[E, A, B] =
    Pure(f)
}
