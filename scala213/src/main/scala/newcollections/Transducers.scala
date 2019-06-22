package newcollections

import scala.collection.View

class Transducer[A, B](inner: Iterable[A] ⇒ View[B]) {
  def map[C](f: B ⇒ C) = new Transducer[A, C](as ⇒ new View.Map(inner(as), f))
  def flatMap[C](f: B ⇒ Iterable[C]) = new Transducer[A,C](as ⇒ new View.FlatMap(inner(as), f))
  def filter(f: B ⇒ Boolean) = new Transducer[A,B](as ⇒ new View.Filter(inner(as), f, false))
  def view(as: Iterable[A]) = inner(as)
  def seq(as: Iterable[A]) = view(as).toSeq
}

object Transducer {
  def create[A] = new Transducer[A, A]({as: Iterable[A] ⇒ as.view})
}

object Blah extends App {
  val t= Transducer.create[Int].map[Double](_ * 0.5).flatMap { d ⇒
    List(s"Hi $d", s"Byte $d")
  }
  val r = t.seq(1 to 5)
  val s = t.seq(6 to 10)
  println(r, s)

}