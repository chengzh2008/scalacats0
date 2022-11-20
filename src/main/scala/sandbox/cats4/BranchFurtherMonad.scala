package sandbox.cats4

import cats.Monad
import sandbox.cats3.Tree
import sandbox.cats3.Leaf
import sandbox.cats3.Branch

object BranchFurtherMonad {
  val treeMonad = new Monad[Tree] {
    def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Leaf(value)         => f(value)
        case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      }

    def pure[A](x: A): Tree[A] = Leaf(x)

    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(f(a))(ei => {
        ei match {
          case Left(a)  => tailRecM(a)(f)
          case Right(b) => Leaf(b)
        }
      })

    def tailRecM1[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      tailRecMHelper(f(a))(f)

    def tailRecMHelper[A, B](
        fa: Tree[Either[A, B]]
    )(fn: A => Tree[Either[A, B]]): Tree[B] =
      fa match {
        case Leaf(Left(a1)) => tailRecM(a1)(fn)
        case Leaf(Right(b)) => Leaf(b)
        case Branch(left, right) => {
          Branch(tailRecMHelper(left)(fn), tailRecMHelper(right)(fn))
        }
      }

  }
}

import BranchFurtherMonad._

object BranchFurtherMonadTest extends App {
  // Branch(Branch(Leaf(99),Leaf(101)),Branch(Leaf(199),Leaf(201)))
  println(
    treeMonad.flatMap(Branch(Leaf(100), Leaf(200)))(x =>
      Branch(Leaf(x - 1), Leaf(x + 1))
    )
  )

}
