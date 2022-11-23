package sandbox.cats8

import scala.concurrent.Future
import cats.Id

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

trait RealUptimeClient extends UptimeClient[Future] {
  def getUptime(hostname: String): Future[Int]
}

trait MockUptimeClient extends UptimeClient[Id] {
  def getUptime(hostname: String): Int
}

class TestUptimeClient(hosts: Map[String, Int]) extends MockUptimeClient {
  def getUptime(hostname: String): Int =
    hosts.getOrElse(hostname, 0)
}
