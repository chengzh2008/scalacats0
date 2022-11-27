package sandbox.cats10

import cats.Semigroup
import cats.syntax.either._
import cats.syntax.semigroup._

// implementation 1) functional wrapper with a class
final case class CheckF[E: Semigroup, A](f: A => Either[E, A]) {
  def apply(a: A): Either[E, A] = f(a)

  def and(that: CheckF[E, A]): CheckF[E, A] =
    CheckF { a =>
      (this(a), that(a)) match {
        case (Left(v1), Left(v2)) => (v1 |+| v2).asLeft
        case (Left(v1), Right(_)) => v1.asLeft
        case (Right(_), Left(v2)) => v2.asLeft
        case (Right(_), Right(_)) => a.asRight
      }
    }
}

// implementation 2) Algebraic Data Type
sealed trait Check[E, A] {
  import Check._

  def and(that: Check[E, A]): Check[E, A] = And(this, that)

  def or(that: Check[E, A]): Check[E, A] = Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] =
    this match {
      case Pure(f) => f(a)
      case And(l, r) =>
        (l(a), r(a)) match {
          case (Left(v1), Left(v2)) => (v1 |+| v2).asLeft
          case (Left(v1), Right(_)) => v1.asLeft
          case (Right(_), Left(v2)) => v2.asLeft
          case (Right(_), Right(_)) => a.asRight
        }
      case Or(l, r) =>
        (l(a), r(a)) match {
          case (Left(v1), Left(v2)) => (v1 |+| v2).asLeft
          case (_, _)               => a.asRight
        }
    }
}

object Check {
  final case class And[E, A](left: Check[E, A], right: Check[E, A])
      extends Check[E, A]
  final case class Or[E, A](left: Check[E, A], right: Check[E, A])
      extends Check[E, A]

  final case class Pure[E, A](f: A => Either[E, A]) extends Check[E, A]

  def pure[E, A](f: A => Either[E, A]): Check[E, A] = Pure(f)
}

object ValidationTest extends App {
  import cats.instances.list._

  // testing for implementation 1
  val a: CheckF[List[String], Int] = CheckF { v =>
    if (v > 2) v.asRight
    else List("Must be > 2").asLeft
  }

  val b: CheckF[List[String], Int] = CheckF { v =>
    if (v < -2) v.asRight
    else List("Must be < -2").asLeft
  }

  val check: CheckF[List[String], Int] = a and b

  // Left(List(Must be < -2))
  println(check(5))
  // Left(List(Must be > 2, Must be < -2))
  println(check(0))

  // testing for implementation 2
  val c: Check[List[String], Int] = Check.pure { v =>
    if (v > 2) v.asRight
    else List("Must be > 2").asLeft
  }
  val d: Check[List[String], Int] = Check.pure { v =>
    if (v < -2) v.asRight
    else List("Must be < -2").asLeft
  }

  val check2 = c.and(d)
  val check3 = c.or(d)
  // Left(List(Must be < -2))
  println(check2.apply(5))
  // Left(List(Must be > 2, Must be < -2))
  println(check2.apply(0))

  // Right(5)
  println(check3.apply(5))
}
