package sandbox.cats7

object ScaffoldingMethod {
  def map[A, B](l: List[A])(f: A => B): List[B] =
    l.foldRight(List.empty[B])((a, acc) => {
      f(a) :: acc
    })

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    l.foldRight(List.empty[B])((a, acc) => {
      f(a) ::: acc
    })

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    l.foldRight(List.empty[A])((a, acc) => {
      f(a) match {
        case true  => a :: acc
        case false => acc
      }
    })

  import cats.Monoid

  def sum[A](l: List[A])(implicit monoid: Monoid[A]): A =
    l.foldRight(monoid.empty)(monoid.combine)
}

import ScaffoldingMethod._

object ScaffoldingMethodTest extends App {
  // List(2, 4, 6)
  println(map(List(1, 2, 3))(_ * 2))

  // List(1, 10, 100, 2, 20, 200, 3, 30, 300)
  println(flatMap(List(1, 2, 3))(a => List(a, a * 10, a * 100)))

  // List(1, 3)
  println(filter(List(1, 2, 3))(_ % 2 == 1))

  // 6
  println(sum(List(1, 2, 3)))
}
