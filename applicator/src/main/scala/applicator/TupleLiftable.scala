package applicator

import scala.annotation.unchecked.uncheckedVariance


trait TupleLiftable[+M[_]] {
  def tupleLift[A,B](t: (M[A],M[B]) @uncheckedVariance): M[(A,B)] @uncheckedVariance
}
