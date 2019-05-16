package scala_async

import scala.concurrent.ExecutionContext.Implicits.global
import scala.async.Async.{async, await}

trait Base[T] {
  def foo: Int
}
trait Selfie {
  self: Base[_] ⇒
  override def foo: Int
}


object Bug {

  def byName[T](f: ⇒ T) = ???

  def asyncThing = async {println("hi")}

  def foo = async {
    val pointless =
      1 match {
        case _ ⇒
          await(asyncThing)
          byName {
            val poorMe = 1
            poorMe
          }
      }
  }


}
