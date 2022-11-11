package sandbox

import cats.implicits._
import cats1._
import PrintableInstances._
import PrintableSyntax._

object Main extends App {
  println("Hello " |+| "Cats!")

  val cat = Cat("kitten", 1, "red")
  cat.print
}
