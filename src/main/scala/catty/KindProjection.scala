package catty

import cats._
import cats.implicits._

object KindProjection {
  /*

  def makeFunctor[U]: Functor[Either[_, U]] = new Functor[Either[?, U]] { // Same: new Functor[λ[α => Either[α, U]]]
    override def map[A, B](fa: Either[A, U])(f: A => B): Either[B, U] = fa match {
      case Left(value: A) => f(value).asLeft
      case Right(value: U) => value.asRight
    }
  }
*/

}
