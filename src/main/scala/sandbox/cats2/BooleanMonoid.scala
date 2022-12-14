package sandbox.cats2

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) = monoid
}

object MonoidBooleanInstances {
  implicit val andMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def empty: Boolean = true
      def combine(x: Boolean, y: Boolean) = x && y
    }

  implicit val orMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def empty: Boolean = false
      def combine(x: Boolean, y: Boolean) = x || y
    }

  implicit val xorMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def empty: Boolean = false
      def combine(x: Boolean, y: Boolean) = (x && !y) || (!x && y)
    }

  implicit val notxorMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def empty: Boolean = true
      def combine(x: Boolean, y: Boolean) = (!x || y) && (x || !y)
    }
}

import MonoidBooleanInstances._

object MonoidBooleanTest extends App {
  println(andMonoid.combine(true, true)) // true
  println(andMonoid.combine(true, false)) // false
  println(andMonoid.combine(false, true)) // false
  println(andMonoid.combine(false, false)) // false

  println(orMonoid.combine(true, true)) // true
  println(orMonoid.combine(true, false)) // true
  println(orMonoid.combine(false, true)) // true
  println(orMonoid.combine(false, false)) // false

  println(xorMonoid.combine(true, true)) // false
  println(xorMonoid.combine(true, false)) // true
  println(xorMonoid.combine(false, true)) // true
  println(xorMonoid.combine(false, false)) // false

  println(notxorMonoid.combine(true, true)) // true
  println(notxorMonoid.combine(true, false)) // false
  println(notxorMonoid.combine(false, true)) // false
  println(notxorMonoid.combine(false, false)) // true
}
