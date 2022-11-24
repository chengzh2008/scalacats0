package sandbox.cats9

import cats.Monoid

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._

object MapReduce {
  def foldMap[A, B: Monoid](as: Vector[A])(fab: A => B): B =
    as.map(fab).foldLeft(Monoid.empty[B])(Monoid.combine[B])

  def parFoldMap[A, B: Monoid](as: Vector[A])(fab: A => B): Future[B] = {
    val numCPU = Runtime.getRuntime.availableProcessors
    val jobs: Iterator[Vector[A]] = as.grouped(numCPU)

    val futures: Iterator[Future[B]] = jobs.map(l => Future { foldMap(l)(fab) })

    Future
      .sequence(futures)
      .map(g => g.foldLeft(Monoid[B].empty)(Monoid.combine[B]))
  }

}

object MapReduceTest extends App {
  import cats.instances.int._
  import cats.instances.string._
  import MapReduce._

  println(foldMap(Vector(1, 2, 3))(identity))
  println(foldMap("hello world!".toVector)(_.toString.toUpperCase))

  val result: Future[Int] = parFoldMap((1 to 1000000).toVector)(identity)
  println(Await.result(result, 1.second))
}
