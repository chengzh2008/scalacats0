package sandbox.cats10

import cats.Semigroup
import cats.syntax.either._
import cats.syntax.semigroup._

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

object ValidationTest extends App {
  import cats.instances.list._

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
}
