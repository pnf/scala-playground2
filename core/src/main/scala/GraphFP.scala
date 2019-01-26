package graph


import scala.annotation.tailrec

object GraphFP {

  type VId = Int

  case class V[X](id: VId, x: X, deps: Set[VId], deprs: Set[VId])

  case class G[X](rid: VId, vs: Map[VId, V[X]]) {
    def root = vs(rid)
    def topo: List[V[X]] = {
      def topo(seen: Set[VId], v: V[X], sofar: List[V[X]]): (Set[VId], List[V[X]]) = {
        val (seen2, sofar2) = v.deps.foldLeft((seen, sofar)) {
          case ((seen: Set[VId], sofar: Seq[V[X]]), i) ⇒
            if (seen.contains(i)) (seen, sofar)
            else topo(seen, vs(i), sofar)
        }
        (seen2 + v.id, sofar2 :+ v)
      }
      topo(Set.empty, root, Nil)._2.reverse
    }
  }

  implicit class GN[X : Numeric](g: G[X]) {
    val N = implicitly[Numeric[X]]

    /**
      * Compute longest path, usually described as:
      *   initialize dist[0,v] for all vertices
      *   visit vs in topological order,
      *   for each edge v -> w, update
      *      dist[0,w] = max(dist[0,w], dist[v,w] + dist[0,v])
      *   In our case, consider the value x of every vertex to be the distance from its parent
      * @return
      */
    def lp: Map[VId, X] = {
      g.topo.foldLeft(Map(g.rid → g.root.x)) {
        case ((m: Map[VId, X]), v: V[X]) ⇒
          val dv = m.getOrElse(v.id,N.zero)
          v.deps.foldLeft(m) {
            case (m, w) ⇒
              val xw = g.vs(w).x
              val dw = m.getOrElse(w, N.zero)
              val dw2 = N.max(N.plus(dv, xw), dw)
              m + (w -> dw2)
          }
      }
    }
  }



  implicit def Tuple2ToTuple2Option[X](vw: (VId, VId)): (VId, VId, Option[X]) = (vw._1, vw._2, None)
  implicit def Tuple3ToTuple2Option[X](vwx: (VId, VId, X)): (VId, VId, Option[X]) = (vwx._1, vwx._2, Some(vwx._3))



  object G {
    /**
      * Convenience method to build a graph out of a sequence of tuples.
      * Each element is one of
      *    (i, i, x)  - set value of vertex i to x
      *    (i, j, x)  - set value of vertex i to x, and add dependency on j
      *    (i, j)     - add dependency of i on j
      */
    def apply[X](es: (VId, VId, Option[X])*): G[X] = {
      @tailrec def build(m: Map[VId, V[Option[X]]], rest: List[(VId, VId, Option[X])]): Map[VId, V[Option[X]]] = rest match {
        case (i, j, xo) :: rest ⇒
          val m2: Map[VId, V[Option[X]]] =
            if(i==j)
              m + (i → m.get(i).fold(V(i, xo, Set.empty, Set.empty)){v ⇒ V(i, xo orElse v.x, v.deps, v.deprs)})
            else {
              m +
                (i → m.get(i).fold(V(i, xo, Set(j), Set.empty)) { v ⇒ V(i, xo orElse v.x, v.deps + j, v.deprs) }) +
                (j → m.get(j).fold(V(j, xo, Set.empty, Set(i))) { v ⇒ V(j, v.x, v.deps, v.deprs + i)})
            }
          build(m2, rest)
        case Nil ⇒
          m
      }
      val m = build(Map.empty, es.toList).
        mapValues(v ⇒ V(v.id, v.x.getOrElse(throw new RuntimeException(s"No value defined for $v")), v.deps, v.deprs))
      G(es.head._1, m)
    }
  }

}
