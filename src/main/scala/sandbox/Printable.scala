package sandbox

trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {
  implicit val stringPritable: Printable[String] =
    new Printable[String] {
      def format(value: String): String = value
    }

  implicit val intPritable: Printable[Int] =
    new Printable[Int] {
      def format(value: Int): String = value.toString()
    }

  implicit val catPrintable = new Printable[Cat] {
    def format(cat: Cat) = {
      val name = Printable.format(cat.name)
      val age = Printable.format(cat.age)
      val color = Printable.format(cat.color)
      s"$name is a $age year-old $color cat."
    }
  }
}

object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)
  // here format(value) or p.format(value) both works
  def print[A](value: A)(implicit p: Printable[A]): Unit = println(
    format(value)
  )
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String = p.format(value)
    // directly use format defined here
    def print(implicit p: Printable[A]): Unit = println(format(p))
  }
}

// use the Printable trait
final case class Cat(name: String, age: Int, color: String)

import PrintableInstances._
import PrintableSyntax._

object PrintableTest extends App {
  println(Printable.format(4))
  Printable.print(4)

  val cat = Cat("Garfield", 41, "ginger and black")
  Printable.print(cat)

  cat.print
}
