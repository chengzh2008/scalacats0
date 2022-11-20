package sandbox.cats5

import cats.data.EitherT
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object TransformRollOut {
  // type Response[A] = Future[Either[String, A]]
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case None        => EitherT.left(Future(s"$autobot unreachable"))
      case Some(value) => EitherT.right(Future(value))
    }

  def canSpecialMove(r1: String, r2: String): Response[Boolean] =
    for {
      n1 <- getPowerLevel(r1)
      n2 <- getPowerLevel(r2)
    } yield n1 + n2 > 15

  def tacticalReport(r1: String, r2: String): String =
    Await.result(canSpecialMove(r1, r2).value, 1.second) match {
      case Left(msg)    => s"Comms error: $msg"
      case Right(true)  => s"$r1 and $r2 are ready to roll out!"
      case Right(false) => s"$r1 and $r2 need a recharge."
    }
}

import TransformRollOut._

object TransformRollOutTest extends App {
  // res13: String = "Jazz and Bumblebee need a recharge."
  println(tacticalReport("Jazz", "Bumblebee"))
  // res14: String = "Bumblebee and Hot Rod are ready to roll out!"
  println(tacticalReport("Bumblebee", "Hot Rod"))
  // res15: String = "Comms error: Ironhide unreachable"
  println(tacticalReport("Jazz", "Ironhide"))
}
