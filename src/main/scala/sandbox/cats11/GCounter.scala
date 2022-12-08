package sandbox.cats11

import cats.syntax.monoid._
import cats.kernel.CommutativeMonoid

trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
  def combine(x: A, y: A): A
  def empty: A
}

object BoundedSemiLatticeInstances {
  implicit val intLattice: BoundedSemiLattice[Int] =
    new BoundedSemiLattice[Int] {
      def combine(x: Int, y: Int): Int = x max y
      def empty: Int = 0
    }

  implicit def setLattice[A]: BoundedSemiLattice[Set[A]] =
    new BoundedSemiLattice[Set[A]] {
      def combine(x: Set[A], y: Set[A]): Set[A] = x union y
      def empty: Set[A] = Set.empty[A]
    }
}

final case class GCounter[A](counters: Map[String, A]) {
  def increment(machine: String, amount: A)(implicit
      ca: CommutativeMonoid[A]
  ): GCounter[A] = {
    val v = amount |+| counters.getOrElse(machine, ca.empty)
    GCounter(counters + (machine -> v))
  }

  def merge(that: GCounter[A])(implicit
      ba: BoundedSemiLattice[A]
  ): GCounter[A] =
    GCounter(counters |+| that.counters)

  def total1(implicit m: CommutativeMonoid[A]): A =
    counters.foldLeft[A](m.empty)((a, b) =>
      b match {
        case (_, n) => a combine n
      }
    )
  def total(implicit m: CommutativeMonoid[A]): A =
    counters.values.foldLeft(m.empty)(m.combine)
}
