package catty

import cats._
import cats.implicits._
import org.scalatest._



class Catty extends FlatSpec with Matchers {

  case class OptionT[F[_], A](value: F[Option[A]])

  implicit def optionTMonad[F[_]](implicit F : Monad[F]) = {
    new Monad[OptionT[F, ?]] {
      def pure[A](a: A): OptionT[F, A] = OptionT(F.pure(Some(a)))
      def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] =
        OptionT {
          F.flatMap(fa.value) {
            case None => F.pure(None)
            case Some(a) => f(a).value
          }
        }

      def tailRecM[A, B](a: A)(f: A => OptionT[F, Either[A, B]]): OptionT[F, B] =
        OptionT {
          F.tailRecM(a)(a0 => F.map(f(a0).value) {
            case None => Either.right[A, Option[B]](None)
            case Some(b0) => b0.map(Some(_))
          })
        }
    }
  }
  // Apparently:
  //   override def combine(x: A => B, y: A => B): A => B =
  //    (a: A) => B.combine(x(a), y(a))
  implicitly[Semigroup[Int => Int]].combine(_ + 1, _ + 2)

  Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4)) should be (List(1, 2, 3, 4, 1, 2))
}
