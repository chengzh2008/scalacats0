package sandbox.cats10

import cats.data.NonEmptyList

object UserValidation {
  type Errors = NonEmptyList[String]

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
import cats.data.Validated
import cats.syntax.apply._ // for mapN
import cats.syntax.validated._ // for validNel

object UserValidationTest extends App {
  val checkUsername: Check[Errors, String, String] =
    Check(longerThan(3) and alphanumeric)

  val splitEmail: Check[Errors, String, (String, String)] =
    Check(str => {
      str.split('@') match {
        case Array(name, domain) =>
          (name, domain).validNel[String]
        case _ =>
          "Must contain a single @ character".invalidNel[(String, String)]
      }
    })

  val checkLeft: Check[Errors, String, String] = Check(longerThan(0))
  val checkRight: Check[Errors, String, String] = Check(
    longerThan(3) and containsOnce('.')
  )

  val joinEmail: Check[Errors, (String, String), String] =
    Check { tup =>
      tup match {
        case (l, r) =>
          (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
      }
    }

  val checkEmail: Check[Errors, String, String] =
    splitEmail andThen joinEmail

  final case class User(username: String, email: String)

  def createUser(username: String, email: String): Validated[Errors, User] =
    (checkUsername(username), checkEmail(email)).mapN(User)

  // Valid(User(abcd,abc@def.com))
  println(createUser("abcd", "abc@def.com"))

  // Invalid(NonEmptyList(Must be longer than 3 characters, Must contain the character . only once))
  println(createUser("abc", "abc@defcom"))

}
