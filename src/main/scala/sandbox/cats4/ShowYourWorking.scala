package sandbox.cats4

import cats.data.Writer
import cats.syntax.applicative._
import cats.instances.vector._
import cats.syntax.writer._

object ShowYourWorking {
  def slowly[A](body: => A) =
    try body
    finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  type Logged[A] = Writer[Vector[String], A]

  def factorialW(n: Int): Logged[Int] = {
    for {
      ans <-
        if (n == 0) {
          1.pure[Logged]
        } else {
          slowly(factorialW(n - 1).map(_ * n))
        }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }
}

import ShowYourWorking._

object ShowYourWorkingTest extends App {
  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._
  import scala.concurrent.duration._

  // fact 0 1
  // fact 0 1
  // fact 1 1
  // fact 1 1
  // fact 2 2
  // fact 2 2
  // fact 3 6
  // fact 3 6
  // fact 4 24
  // fact 4 24
  // fact 5 120
  // fact 5 120
  Await.result(
    Future.sequence(
      Vector(
        Future(factorial(5)),
        Future(factorial(5))
      )
    ),
    5.seconds
  )

  // Vector(fact 0 1, fact 1 1, fact 2 2, fact 3 6, fact 4 24, fact 5 120)
  // Vector(fact 0 1, fact 1 1, fact 2 2, fact 3 6, fact 4 24, fact 5 120)
  Await.result(
    Future.sequence(
      Vector(
        Future({
          println(factorialW(5).written)
        }),
        Future(println(factorialW(5).written))
      )
    ),
    5.seconds
  )
}
