package sandbox.cats8

import cats.Applicative
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.functor._

// import scala.concurrent.Future
// import scala.concurrent.ExecutionContext.Implicits.global
// import cats.instances.future._

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}

object UptimeServiceTest extends App {
  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keySet.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

  testTotalUptime()
}
