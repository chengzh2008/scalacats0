package sandbox.cats4

import cats.data.Reader
import cats.syntax.applicative._

final case class Db(
    usernames: Map[Int, String],
    passwords: Map[String, String]
)

object HackingReader {
  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => {
      db.usernames.get(userId)
    })

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => {
      db.passwords.get(username).contains(password)
    })

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      name <- findUsername(userId)
      good <- name
        .map { username =>
          checkPassword(username, password)
        }
        .getOrElse(false.pure[DbReader])
    } yield good
}

import HackingReader._

object HackingReaderTest extends App {
  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  // true
  println(checkLogin(1, "zerocool").run(db))

  // false
  println(checkLogin(4, "davinci").run(db))

}
