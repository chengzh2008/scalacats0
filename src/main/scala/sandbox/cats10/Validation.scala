package sandbox.cats10

import cats.data.Validated._

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
  val c: Predicate[List[String], Int] = Predicate { v =>
    if (v > 2) Valid(v)
    else Invalid(List("Must be > 2"))
  }
  val d: Predicate[List[String], Int] = Predicate.apply { v =>
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
