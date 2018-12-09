package zio

import scalaz.zio._
import scalaz.zio.console._
import scala.concurrent.duration.DurationInt

object FiberTest extends App {

  def foo(i: Int, s: String) = for {
    _ ← putStrLn(s"Entering $s")
    _ ← IO.sleep(i seconds)
    _ ← putStrLn( s"Leaving $s")
  } yield i

  override def run(args: List[String]): IO[Nothing, FiberTest.ExitStatus] = {

    for {
      f1 ← foo(1,"A").fork
      f2 ← foo(1,"B").fork
      v1 ← f1.join
      v2 ← f2.join
      _ ← putStrLn(s"Got ${v1 + v2}")
    } yield ExitStatus.ExitNow(0)

  }

}
