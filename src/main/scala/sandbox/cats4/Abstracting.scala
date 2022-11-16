package sandbox.cats4

import cats.MonadError
import scala.util.Try
import cats.instances.try_._

object Abstracting {
  def validateAdult[F[_]](age: Int)(implicit
      me: MonadError[F, Throwable]
  ): F[Int] =
    me.ensure(me.pure(age))(
      new IllegalArgumentException("Age must be greater than or equal to 18")
    )(_ >= 18)
}

object AbstractingTest extends App {
  // Success(18)
  println(Abstracting.validateAdult[Try](18))
  // Failure(java.lang.IllegalArgumentException: Age must be greater than or equal to 18)

  type ExceptionOr[A] = Either[Throwable, A]
  // Left(java.lang.IllegalArgumentException: Age must be greater than or equal to 18)
  println(Abstracting.validateAdult[ExceptionOr](-1))
}
