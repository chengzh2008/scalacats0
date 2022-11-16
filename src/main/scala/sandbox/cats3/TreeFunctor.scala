package sandbox.cats3

import cats.Functor

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object TreeFunctorInstances {
  implicit def treeFunctor: Functor[Tree] =
    new Functor[Tree] {
      def map[A, B](value: Tree[A])(f: A => B): Tree[B] =
        value match {
          case Branch(left, right) =>
            Branch(map(left)(f), map(right)(f))
          case Leaf(v) => Leaf(f(v))
        }

    }
}

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](v: A): Tree[A] = Leaf(v)
}

import TreeFunctorInstances._

object TreeFunctorTest extends App {
  val t = Tree.branch(Tree.branch(Tree.leaf(3), Tree.leaf(5)), Tree.leaf(6))
  // Branch(Branch(Leaf(6),Leaf(10)),Leaf(12))
  println(treeFunctor.map(t)(_ * 2))
}
