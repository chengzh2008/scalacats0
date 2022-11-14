package sandbox.cats4

trait Monad[F[_]] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](v: F[A])(f: A => F[B]): F[B]

  def map[A, B](v: F[A])(f: A => B): F[B] =
    flatMap(v)(a => pure(f(a)))
}

object MonadInstances {
  implicit def optionMonad: Monad[Option] =
    new Monad[Option] {
      def pure[A](a: A): Option[A] = Some(a)
      def flatMap[A, B](v: Option[A])(f: A => Option[B]): Option[B] =
        v match {
          case Some(value) => f(value)
          case None        => None
        }
    }
}

import MonadInstances._

object FunctorMonadTest extends App {
  println(optionMonad.pure(1))
  println(optionMonad.map(Some(1))(n => n * 2))
}
