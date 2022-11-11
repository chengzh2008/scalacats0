package sandbox

import cats.Eq
import cats.syntax.eq._

package object catEq {
  implicit val catEqual: Eq[Cat] =
    Eq.instance[Cat] { (cat1, cat2) =>
      cat1.name === cat2.name &&
      cat1.age === cat2.age &&
      cat1.color === cat2.color
    }
}

import catEq._

object CatEqTest extends App {
  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 32, "orange and black")
  println(cat1 === cat2)

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]
  println(optionCat1 === optionCat2)
}
