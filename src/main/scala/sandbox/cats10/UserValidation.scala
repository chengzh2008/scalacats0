package sandbox.cats10

import cats.data.NonEmptyList
import cats.data.Kleisli

object UserValidation {
  type Errors = NonEmptyList[String]

  type Result[A] = Either[Errors, A]

  type Check[A, B] = Kleisli[Result, A, B]

  def check[A, B](f: A => Result[B]): Check[A, B] = Kleisli(f)

  def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] =
    Kleisli[Result, A, A](pred.run)

  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.size > n
    )

  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      err = error(s"Must be all alphanumeric characters"),
      fn = str => str.forall(_.isLetterOrDigit)
    )

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char"),
      str => str.contains(char)
    )

  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.filter(_ == char).size == 1
    )
}

import UserValidation._
import cats.syntax.apply._ // for mapN
import cats.syntax.either._ // for validNel

object UserValidationTest extends App {
  val checkUsername: Check[String, String] =
    checkPred(longerThan(3) and alphanumeric)

  val splitEmail: Check[String, (String, String)] =
    check(str =>
      str.split('@') match {
        case Array(name, domain) => (name, domain).rightNel
        case _                   => "Must contain a single @ character".leftNel
      }
    )

  val checkLeft: Check[String, String] = checkPred(longerThan(0))
  val checkRight: Check[String, String] = checkPred(
    longerThan(3) and containsOnce('.')
  )

  val joinEmail: Check[(String, String), String] =
    check { tup =>
      tup match {
        case (l, r) =>
          (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
      }
    }

  val checkEmail: Check[String, String] =
    splitEmail andThen joinEmail

  final case class User(username: String, email: String)

  def createUser(username: String, email: String): Result[User] =
    (checkUsername(username), checkEmail(email)).mapN(User)

  // Right(User(abcd,abc@def.com))
  println(createUser("abcd", "abc@def.com"))

  // Left(NonEmptyList(Must be longer than 3 characters))
  println(createUser("abc", "abc@defcom"))

}
