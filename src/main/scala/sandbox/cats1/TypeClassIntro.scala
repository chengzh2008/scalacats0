package cats1

// JSON AST
sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
final case object JsNull extends Json

// JsonWriter is a typeclass
trait JsonWriter[A] {
  def write(value: A): Json
}

final case class Person(name: String, email: String)

// typeclass instances
object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] =
    new JsonWriter[String] {
      def write(value: String): Json = JsString(value)
    }

  implicit val personWriter: JsonWriter[Person] =
    new JsonWriter[Person] {
      def write(value: Person): Json = JsObject(
        Map("name" -> JsString(value.name), "email" -> JsString(value.email))
      )
    }

  // typeclass composition
  implicit def optionWriter[A](implicit
      writer: JsonWriter[A]
  ): JsonWriter[Option[A]] =
    new JsonWriter[Option[A]] {
      def write(option: Option[A]): Json =
        option match {
          case Some(value) => writer.write(value)
          case None        => JsNull
        }
    }
}

// typeclass use 1
object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = w.write(value)
}

// typeclass use 2
object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json = w.write(value)
  }
}

// 4 ways to package type class instances
// For our purposes, we can package type class instances in roughly four ways:
//
// by placing them in an object such as JsonWriterInstances;
// by placing them in a trait;
// by placing them in the companion object of the type class;
// by placing them in the companion object of the parameter type.

import cats.implicits._
import JsonWriterInstances._
import JsonSyntax._

object TypeClassTest extends App {
  println("Hello " |+| "Cats!")
  // Hello Cats!
  val person = Person("cheng", "abcdef@dot.com")
  // use typeclass method 1
  // JsObject(Map(name -> JsString(cheng), email -> JsString(abcdef@dot.com)))
  println(Json.toJson(person))
  // use typeclass method 2
  // JsObject(Map(name -> JsString(cheng), email -> JsString(abcdef@dot.com)))
  println(person.toJson)

  // implicitely
  // cats1.JsonWriterInstances$$anon$1@44db5e98
  println(implicitly[JsonWriter[String]])

  // JsObject(Map(name -> JsString(cheng), email -> JsString(abcdef@dot.com)))
  println(Option(person).toJson)
  // JsNull
  println((None: Option[Person]).toJson)
  // JsNull
  println((None: Option[String]).toJson)
}
