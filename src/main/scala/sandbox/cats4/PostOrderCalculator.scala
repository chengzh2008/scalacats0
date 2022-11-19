package sandbox.cats4

import cats.data.State
import cats.syntax.applicative._

object PostOrderCalculator {
  type CalcState[A] = State[List[Int], A]

  def isBinaryOperator(sym: String): Boolean =
    sym == "+" || sym == "*"

  def operator(fn: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] { st =>
      st match {
        case a :: b :: cs =>
          val ans = fn(a, b)
          (ans :: cs, ans)
        case _ => sys.error("Fail!")
      }
    }

  def operand(n: Int): CalcState[Int] =
    State[List[Int], Int] { st =>
      (n :: st, n)
    }

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (a, b) =>
      a.flatMap(_ => evalOne(b))
    }

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value
}

import PostOrderCalculator._

object PostOrderCalculatorTest extends App {
  // 42
  println(evalOne("42").runA(Nil).value)

  val program = for {
    _ <- evalOne("10")
    _ <- evalOne("12")
    ans <- evalOne("*")
  } yield ans

  // 120
  println(program.runA(Nil).value)

  val multistageProgram = evalAll(List("1", "2", "+", "3", "*"))
  // 9
  println(multistageProgram.runA(Nil).value)

  val biggerProgram = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans

  // 21
  println(biggerProgram.runA(Nil).value)

  val calculator = evalInput("1 2 + 3 *")
  // 9
  println(calculator)
}
