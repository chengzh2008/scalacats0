package sandbox.cats4

object IdMonadInstances {
  type Id[A] = A

  implicit def idMonad: Monad[Id] =
    new Monad[Id] {
      def pure[A](v: A): Id[A] = v
      def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
    }
}

import IdMonadInstances._

object IdMonad extends App {
  val idm = idMonad.pure("abc")
  println(idm)

  println(idMonad.flatMap(idm)(_ ++ " def"))
}
