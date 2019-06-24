package newcollections

import scala.collection.View


class Viewer[X, B](val xToViewB: X ⇒ View[B]) {
  def map[C](f: B ⇒ C) = new Viewer[X, C](as ⇒ new View.Map(xToViewB(as), f))
  def flatMap[C](f: B ⇒ Iterable[C]) = new Viewer[X,C](as ⇒ new View.FlatMap(xToViewB(as), f))
  def filter(f: B ⇒ Boolean) = new Viewer[X,B](as ⇒ new View.Filter(xToViewB(as), f, false))
  def distinctBy[C](f: B => C) = new Viewer[X, B](as ⇒ new View.DistinctBy(xToViewB(as), f))
  def drop(n: Int) = new Viewer[X, B](as ⇒ new View.Drop[B](xToViewB(as), n))
  def dropRight(n: Int) = new Viewer[X, B](as ⇒ new View.DropRight[B](xToViewB(as), n))
  def dropWhile(f: B ⇒ Boolean)= new Viewer[X, B](as ⇒ new View.DropWhile[B](xToViewB(as),f))
  def take(n: Int) = new Viewer[X, B](as ⇒ new View.Take[B](xToViewB(as), n))
  def takeRight(n: Int) = new Viewer[X, B](as ⇒ new View.TakeRight[B](xToViewB(as), n))
  def takeWhile(f: B ⇒ Boolean)= new Viewer[X, B](as ⇒ new View.TakeWhile[B](xToViewB(as),f))
  def scanLeft[C](z: C)(op: (C, B) => C) = new Viewer[X, C](as ⇒ new View.ScanLeft(xToViewB(as), z, op))
  def collect[C](pf: PartialFunction[B, C]) = new Viewer[X, C](as ⇒ new View.Collect(xToViewB(as), pf))

  def ++[Y](t2: Viewer[Y, B]) = new Viewer[(X,Y), B]({
    case (x, y) ⇒ new View.Concat(xToViewB(x), t2.xToViewB(y))
    }
  )

  def apply(x: X) = xToViewB(x)
}

object Viewer {
  def create[A] = new Viewer[View[A], A](identity)

  implicit class Viewer1[A1, B](t1: Viewer[View[A1], B]) {
    def apply(i1: Seq[A1]) = t1(i1.view).toSeq
  }
  implicit class Viewer2[A1,A2, B](t2: Viewer[(View[A1], View[A2]), B]) {
    def apply(s1: Seq[A1], s2: Seq[A2]) = t2((s1.view, s2.view)).toSeq
    def apply(v1: View[A1], v2: View[A2]) = t2((v1, v2))
  }
  implicit class Viewer3[A1, A2, A3, B](t3: Viewer[((View[A1], View[A2]), View[A3]), B]) {
    def apply(s1: Seq[A1], s2: Seq[A2], s3: Seq[A3]) = t3(((s1.view, s2.view), s3.view)).toSeq
  }


}




object Blah extends App {
  val t= Viewer.create[Int].map[Double](_ * 0.5).flatMap { d ⇒
    List(s"Hi $d", s"Byte $d")
  }

  val t2 = t ++ t

  val r = t((1 to 5).view).toSeq
  val s = t2((1 to 5), (10 to 13))
  println(r, s)

}
