package sandbox.cats10

import cats.Semigroup
import cats.syntax.semigroup._
import cats.data.Validated
import cats.data.Validated._
import cats.syntax.apply._
import cats.syntax.validated._

// implementation 1) functional wrapper with a class
final case class CheckF[E: Semigroup, A](f: A => Validated[E, A]) {
  def apply(a: A): Validated[E, A] = f(a)

  def and(that: CheckF[E, A]): CheckF[E, A] =
    CheckF { a =>
      (this(a), that(a)).mapN((_, _) => a)
    }
}

// implementation 2) Algebraic Data Type
sealed trait Predicate[E, A] {
  import Predicate._

  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case Pure(f) => f(a)

      case And(l, r) =>
        (l(a), r(a)).mapN((_, _) => a)

      case Or(l, r) =>
        l(a) match {
          case Valid(a) => Valid(a)
          case Invalid(e1) => {
            r(a) match {
              case Valid(a)    => Valid(a)
              case Invalid(e2) => Invalid(e1 |+| e2)
            }
          }
        }
    }
}

object Predicate {
  final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A])
      extends Predicate[E, A]

  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A])
      extends Predicate[E, A]

  final case class Pure[E, A](f: A => Validated[E, A]) extends Predicate[E, A]

  def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)

  def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] =
    Pure { a =>
      if (fn(a)) a.valid
      else err.invalid
    }
}
