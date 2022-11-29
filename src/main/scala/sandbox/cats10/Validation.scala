package sandbox.cats10

import cats.Semigroup
import cats.syntax.semigroup._
import cats.data.Validated
import cats.data.Validated._
import cats.syntax.apply._

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
      case Predicate.Pure(f) => f(a)
      case And(l, r) =>
        (l(a), r(a)).mapN((_, _) => a)
      case Or(l, r) =>
        l(a) match {
          case Valid(a) => Valid(a)
          case Invalid(e1) => {
            r(a) match {
              case Valid(a)    => Valid(a)
              case Invalid(e2) => Invalid(e1.combine(e2))
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

  def pure[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)
}

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

  final case class Pure[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      pred(a)
  }

  def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
    Pure(pred)
}

object ValidationTest extends App {
  import cats.instances.list._

  // testing for implementation 1
  val a: CheckF[List[String], Int] = CheckF { v =>
    if (v > 2) Valid(v)
    else Invalid(List("Must be > 2"))
  }

  val b: CheckF[List[String], Int] = CheckF { v =>
    if (v < -2) Valid(v)
    else Invalid(List("Must be < -2"))
  }

  val check: CheckF[List[String], Int] = a and b

  // Invalid(List(Must be < -2))
  println(check(5))
  // Invalid(List(Must be > 2, Must be < -2))
  println(check(0))

  // testing for implementation 2
  val c: Predicate[List[String], Int] = Predicate.pure { v =>
    if (v > 2) Valid(v)
    else Invalid(List("Must be > 2"))
  }
  val d: Predicate[List[String], Int] = Predicate.pure { v =>
    if (v < -2) Valid(v)
    else Invalid(List("Must be < -2"))
  }

  val check2 = c.and(d)
  val check3 = c.or(d)
  // Invalid(List(Must be < -2))
  println(check2.apply(5))
  // Invalid(List(Must be > 2, Must be < -2))
  println(check2.apply(0))

  // Valid(5)
  println(check3.apply(5))
}
