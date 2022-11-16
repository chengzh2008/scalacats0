package sandbox.cats3

trait Codec[A] { self =>
  def encode(v: A): String
  def decode(v: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] =
    new Codec[B] {
      def encode(v: B): String = self.encode(enc(v))
      def decode(v: String): B = dec(self.decode(v))
    }
}

object Codec {
  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A =
    c.decode(value)
}

import cats1.Box

object CodecInstances {
  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      def encode(value: String): String = value
      def decode(value: String): String = value
    }

  // manually implement doubleCodec
  // implicit val doubleCodec: Codec[Double] =
  //   new Codec[Double] {
  //     def encode(v: Double): String = v.toString
  //     def decode(v: String): Double = v.toDouble
  //   }

  // get doubleCodec by using stringCodec and imap
  implicit val doubleCodec: Codec[Double] =
    stringCodec.imap(_.toDouble, _.toString)

  implicit val booleanCodec: Codec[Boolean] =
    stringCodec.imap(_.toBoolean, _.toString)

  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
    stringCodec.imap(v => Box(c.decode(v)), v => c.encode(v.value))
}

import CodecInstances._

object CodecTest extends App {
  // 1234.4
  println(Codec.encode(1234.4))
  // 1234.0
  println(Codec.decode[Double]("1234"))
  // 13.4
  println(Codec.encode(Box(13.4)))
  // Box(123.4)
  println(Codec.decode[Box[Double]]("123.4"))

}
