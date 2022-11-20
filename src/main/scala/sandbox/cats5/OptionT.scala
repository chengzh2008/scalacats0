package sandbox.cats5

import cats.data.OptionT
import cats.data.Writer
import cats.instances.list._
import cats.syntax.applicative._

object OptionTT {
  type Logged[A] = Writer[List[String], A]

  // Methods generally return untransformed stacks:
  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None      => Writer(List(s"Failed on $str"), None)
    }

// Consumers use monad transformers locally to simplify composition:
  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    import cats.data.OptionT

    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c

    result.value
  }

}

import OptionTT._

object OptionTTest extends App {
  type ListOption[A] = OptionT[List, A]

  val r1: ListOption[Int] = OptionT(List(Option(10)))
  val r2: ListOption[Int] = 32.pure[ListOption]

  // OptionT(List(Some(42)))
  println(
    r1.flatMap(x => r2.map(y => x + y))
  )

  // WriterT((List(Read 1, Read 2, Read 3),Some(6)))
  println(addAll("1", "2", "3"))
}
