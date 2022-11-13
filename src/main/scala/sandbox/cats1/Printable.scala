package cats1

trait Printable[A] { self =>
  def format(value: A): String

  def contramap[B](f: B => A): Printable[B] =
    new Printable[B] {
      def format(v: B): String =
        self.format(f(v))
    }
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

final case class Box[A](value: A)

// implement BoxPrintable by using contramap and Printable
object BoxPrintableInstance {
  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap[Box[A]](_.value)
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
import BoxPrintableInstance._

object PrintableTest extends App {
  println(Printable.format(4))
  Printable.print(4)

  val cat = Cat("Garfield", 41, "ginger and black")
  Printable.print(cat)

  cat.print

  val b1 = Box(3)
  Printable.print(b1)
  val b2 = Box("Hello world!")
  Printable.print(b2)
}
