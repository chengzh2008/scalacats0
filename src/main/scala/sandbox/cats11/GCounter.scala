package sandbox.cats11

import cats.kernel.CommutativeMonoid
import cats.instances.map._
import cats.syntax.semigroup._
import cats.syntax.foldable._

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

trait GGCounter[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit
      m: CommutativeMonoid[V]
  ): F[K, V]

  def merge(f1: F[K, V], f2: F[K, V])(implicit
      b: BoundedSemiLattice[V]
  ): F[K, V]

  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}

object GGCounter {
  def apply[F[_, _], K, V](implicit
      counter: GGCounter[F, K, V]
  ): GGCounter[F, K, V] = counter
}

object GGCounterInstance {
  implicit def mapGGCounterInsntace[K, V]: GGCounter[Map, K, V] =
    new GGCounter[Map, K, V] {
      def increment(
          f: Map[K, V]
      )(k: K, v: V)(implicit m: CommutativeMonoid[V]): Map[K, V] = {
        val nv = v |+| f.getOrElse(k, m.empty)
        f + (k -> nv)
      }

      def merge(f1: Map[K, V], f2: Map[K, V])(implicit
          b: BoundedSemiLattice[V]
      ): Map[K, V] = {
        f1 |+| f2
      }

      def total(f: Map[K, V])(implicit m: CommutativeMonoid[V]): V =
        f.values.toList.combineAll
    }

  implicit def ggcounterInstance[F[_, _], K, V](implicit
      kvs: KVStore[F],
      km: CommutativeMonoid[F[K, V]]
  ) =
    new GGCounter[F, K, V] {
      def increment(f: F[K, V])(k: K, v: V)(implicit
          m: CommutativeMonoid[V]
      ): F[K, V] =
        kvs.put(f)(k, v)
      def merge(f1: F[K, V], f2: F[K, V])(implicit
          b: BoundedSemiLattice[V]
      ): F[K, V] =
        f1 |+| f2
      def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V =
        kvs.values(f).combineAll
    }

}

trait KVStore[F[_, _]] {
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]
  def get[K, V](f: F[K, V])(k: K): Option[V]
  def getOrElse[K, V](f: F[K, V])(k: K, v: V): V =
    get(f)(k).getOrElse(v)
  def values[K, V](f: F[K, V]): List[V]

}

object KVStoreInstance {
  implicit val mapKVStoreInstance: KVStore[Map] = {
    new KVStore[Map] {
      def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] =
        f + (k -> v)
      def get[K, V](f: Map[K, V])(k: K): Option[V] =
        f.get(k)
      def values[K, V](f: Map[K, V]): List[V] =
        f.values.toList
    }
  }
}

object GCounter extends App {
  import GGCounterInstance._
  import BoundedSemiLatticeInstances._
  import cats.instances.int._

  val g1 = Map("a" -> 7, "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)
  //
  val counter = GGCounter[Map, String, Int]
  val merged = counter.merge(g1, g2)
  println(merged)
  println(merged.values)
  val total = counter.total(merged)(catsKernelStdGroupForInt)
  // 12
  println(total)
}
