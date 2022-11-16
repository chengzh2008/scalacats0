package sandbox.cats2

object MonoidSetInstances {
  implicit def unionMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def empty = Set.empty[A]
      def combine(x: Set[A], y: Set[A]): Set[A] = x union y
    }

  implicit def symDiffMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def empty = Set.empty[A]
      def combine(x: Set[A], y: Set[A]): Set[A] = (x diff y) union (y diff x)
    }
}

object SemigroupInstances {
  implicit def intersectionSemigroup[A]: Semigroup[Set[A]] =
    new Semigroup[Set[A]] {
      def combine(x: Set[A], y: Set[A]): Set[A] = x intersect y
    }
}

import MonoidSetInstances._
import SemigroupInstances._

object MonoidSetTest extends App {
  val intUnionSetMonoid = unionMonoid[Int]
  val set1 = Set(1, 3)
  val set2 = Set(1, 5)
  // Set(1, 3, 5)
  println(intUnionSetMonoid.combine(set1, set2))

  val intIntersectionSetMonoid = intersectionSemigroup[Int]
  // Set(1)
  println(intIntersectionSetMonoid.combine(set1, set2))

  val intSymDiffSetMonoid = symDiffMonoid[Int]
  // Set(3, 5)
  println(intSymDiffSetMonoid.combine(set1, set2))
}
