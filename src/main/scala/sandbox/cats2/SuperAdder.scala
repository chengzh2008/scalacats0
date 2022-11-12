package sandbox.cats2

import cats.Monoid
import cats.syntax.semigroup._

object SuperAdder {
  def add[A: Monoid](items: List[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)
}

case class Order(totalCost: Double, quantity: Double)

object MonoidInstance {
  implicit val orderMonoid: Monoid[Order] =
    new Monoid[Order] {
      def empty = Order(0, 0)
      def combine(x: Order, y: Order) =
        Order(x.totalCost |+| y.totalCost, x.quantity |+| y.quantity)
    }
}

import MonoidInstance._

object SuperAdderTest extends App {
  val l1 = List(1, 2, 4, 5)
  println(SuperAdder.add(l1))

  val l2 = List(Some(1), Some(2), None, Some(5))
  println(SuperAdder.add(l2))

  val l3 = List(Order(1, 2), Order(3, 5))
  println(SuperAdder.add(l3))
}
