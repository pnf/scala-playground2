package newcollections

import scala.collection.View


class Transducer[X, B](val xToViewB: X ⇒ View[B]) {
  def map[C](f: B ⇒ C) = new Transducer[X, C](as ⇒ new View.Map(xToViewB(as), f))
  def flatMap[C](f: B ⇒ Iterable[C]) = new Transducer[X,C](as ⇒ new View.FlatMap(xToViewB(as), f))
  def filter(f: B ⇒ Boolean) = new Transducer[X,B](as ⇒ new View.Filter(xToViewB(as), f, false))
  def distinctBy[C](f: B => C) = new Transducer[X, B](as ⇒ new View.DistinctBy(xToViewB(as), f))
  def drop(n: Int) = new Transducer[X, B](as ⇒ new View.Drop[B](xToViewB(as), n))
  def dropRight(n: Int) = new Transducer[X, B](as ⇒ new View.DropRight[B](xToViewB(as), n))
  def dropWhile(f: B ⇒ Boolean)= new Transducer[X, B](as ⇒ new View.DropWhile[B](xToViewB(as),f))
  def take(n: Int) = new Transducer[X, B](as ⇒ new View.Take[B](xToViewB(as), n))
  def takeRight(n: Int) = new Transducer[X, B](as ⇒ new View.TakeRight[B](xToViewB(as), n))
  def takeWhile(f: B ⇒ Boolean)= new Transducer[X, B](as ⇒ new View.TakeWhile[B](xToViewB(as),f))
  def scanLeft[C](z: C)(op: (C, B) => C) = new Transducer[X, C](as ⇒ new View.ScanLeft(xToViewB(as), z, op))
  def collect[C](pf: PartialFunction[B, C]) = new Transducer[X, C](as ⇒ new View.Collect(xToViewB(as), pf))

  def ++[Y](t2: Transducer[Y, B]) = new Transducer[(X,Y), B]({
    case (x, y) ⇒ new View.Concat(xToViewB(x), t2.xToViewB(y))
    }
  )

  def apply(x: X) = xToViewB(x)
}

object Transducer {
  def create[A] = new Transducer[View[A], A](identity)


}




object Blah extends App {
  val t= Transducer.create[Int].map[Double](_ * 0.5).flatMap { d ⇒
    List(s"Hi $d", s"Byte $d")
  }

  val t2 = t ++ t

  val r = t((1 to 5).view).toSeq
  val s = t2(((1 to 5).view, (10 to 13).view)).toSeq
  println(r, s)

}
