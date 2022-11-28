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
sealed trait Check[E, A] {
  import Check._

  def and(that: Check[E, A]): Check[E, A] = And(this, that)

  def or(that: Check[E, A]): Check[E, A] = Or(this, that)

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
              case Invalid(e2) => Invalid(e1.combine(e2))
            }
          }
        }
    }
}

object Check {
  final case class And[E, A](left: Check[E, A], right: Check[E, A])
      extends Check[E, A]
  final case class Or[E, A](left: Check[E, A], right: Check[E, A])
      extends Check[E, A]

  final case class Pure[E, A](f: A => Validated[E, A]) extends Check[E, A]

  def pure[E, A](f: A => Validated[E, A]): Check[E, A] = Pure(f)
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
  val c: Check[List[String], Int] = Check.pure { v =>
    if (v > 2) Valid(v)
    else Invalid(List("Must be > 2"))
  }
  val d: Check[List[String], Int] = Check.pure { v =>
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
