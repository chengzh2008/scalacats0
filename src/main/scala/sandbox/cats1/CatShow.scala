package cats1

import cats.Show
import cats.instances.int._ // for Show
import cats.instances.string._ // for Show
import cats.syntax.show._ // for show

package object catShow {
  implicit val catShow: Show[Cat] = Show.show[Cat] { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show
    s"$name is a $age year-old $color cat."
  }
}

import catShow._

object CatShowTest extends App {
  val showInt = Show.apply[Int]
  // 123
  println(123.show)
  // 123
  println(showInt.show(123))

  val cat = Cat("Garfield", 41, "ginger and black")
  // Garfield is a 41 year-old ginger and black cat.
  println(catShow.show(cat))
}
