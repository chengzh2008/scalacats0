package sandbox

import cats.Show
import cats.instances.int._    // for Show
import cats.instances.string._ // for Show
import cats.syntax.show._      // for show

final case class Cat(name: String, age: Int, color: String)

implicit val catShow: Show[Cat] = Show.show[Cat] { cat =>
  val name  = cat.name.show
  val age   = cat.age.show
  val color = cat.color.show
  s"$name is a $age year-old $color cat."
}

object CatShow extends App {
  val showInt = Show.apply[Int]
  123.show
  showInt.show(123)
}
