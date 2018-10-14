package applicator

import cats.{Invariant, Semigroupal}

import scala.collection.mutable
import scala.language.higherKinds
import scala.languageFeature.implicitConversions
import cats.effect.ConcurrentEffect
import cats.instances.list._
import cats.effect._
import cats.temp.par._
import fetch.Fetch
import cats.Semigroupal


object LiftFetchTuples {
  import scala.language.experimental.macros
  import scala.reflect.macros.blackbox.Context

  def apply[M[_], T](expr: Fetch[M,T])(implicit semigroupal: Semigroupal[Fetch[M, ?]], invariant: Invariant[Fetch[M,?]]): Fetch[M,T] =  macro liftTuplesImpl[M, T]

  def liftTuplesImpl[M[_], T](c: Context)(expr: c.Tree)( semigroupal: c.Tree,  invariant: c.Tree)(implicit M: c.WeakTypeTag[M[_]], T:c.WeakTypeTag[T]) = {

    import c.universe._

    val flatMapName = TermName("flatMap")
    val mapName = TermName("map")

    class TupleLiftingTransformer extends Transformer {

      private object PossiblyNestedMap {
        def unapply(tree: Tree): Option[Tree] = tree match {

          // Convert
          //   vm.flatMap { v => wm.mapOrFlatMap { w => expr } }
          // to
          //   tuple2((vm,wm)).mapOrFlatMap { z$123 =>
          //     val v = z$123._1
          //     val w = z$123._2
          //     expr
          //   }
          // as long as wm does not incorporate v.
          //
          // This is done recurmively, so the inner wm.mapOrFlatMap may be the result of prior
          // lifting.  For example,
          //   for (v <- vm; w <- wm; x <- xm) yield expr
          // might result in
          // tuple2((vm,tuple2((wm, xm)))).map { z$234 =>
          //    v = z$234._1
          //    z$123 = z$234._2
          //    {
          //      w = z$123._1
          //      x = z$123._2
          //      expr
          //    }
          // }
          //
          //
            /*

cats.syntax.`package`.all.toFunctorOps[[A]fetch.Fetch[F,A], (fetchy.Fetchy.Author, fetchy.Fetchy.Quality)]
  (
  cats.Semigroupal.tuple2[[A]fetch.Fetch[F,A], fetchy.Fetchy.Author, fetchy.Fetchy.Quality]
    (
      cats.syntax.`package`.all.toFlatMapOps[[A]fetch.Fetch[F,A], fetchy.Fetchy.Author]
        (
          Fetchy.getAuthor[F](p.id)(evidence$47, evidence$48)
        )
        (fetch.`package`.fetchM[F](evidence$47) )
        .self,
      cats.syntax.`package`.all.toFunctorOps[[A]fetch.Fetch[F,A], fetchy.Fetchy.Quality]
        (
          Fetchy.getQuality[F](p.content)(evidence$47, evidence$48)
        )
        (fetch.`package`.fetchM[F](evidence$47)
        ).self
    )
    (
      fetch.`package`.fetchM[F](evidence$47), fetch.`package`.fetchM[F](evidence$47)
    )
  )
  (fetch.`package`.fetchM[F](evidence$47))
  .map[(fetchy.Fetchy.Post, fetchy.Fetchy.Author, fetchy.Fetchy.Quality)]
    (((x$11: (fetchy.Fetchy.Author, fetchy.Fetchy.Quality)) => {
       val a: fetchy.Fetchy.Author = x$11._1;
       val q: fetchy.Fetchy.Quality = x$11._2;
       scala.Tuple3.apply[fetchy.Fetchy.Post, fetchy.Fetchy.Author, fetchy.Fetchy.Quality](p, a, q)
    }))

 */

          case Apply(TypeApply(Select(vx, TermName("flatMap")), outerMethTypeParam),
          (oldClosure@Function(vValDef :: Nil,
          PossiblyNestedMap(Apply(TypeApply(Select(wx, innerMeth), innerMethTypeParam),
          (oldInnerClosure@Function(wValDef :: Nil, expr)) :: Nil)))) :: Nil)
            if (innerMeth == flatMapName || innerMeth == mapName) ⇒

            /*
            // Typically, vm will look like this
             cats.syntax.`package`.all.toFlatMapOps[[A]fetch.Fetch[F,A], fetchy.Fetchy.Author]
                         (Fetchy.getAuthor[F](p.id)(evidence$47, evidence$48))  // evidence that F is ConcurrentEffect and Par
                         (fetch.`package`.fetchM[F](evidence$47)) // Implicit FetchMonad created using Evidence that F is Monad


             We want to call cats.SemiGroupal.tuple2 on two arguments F[_] where F is Semigroupal and invariant
             */

            // It's possible that our types have been converted to Functor.Ops or somesuch, in which case
            // we can recover the original Fetch with .self
            val vmself = c.typecheck(q"$vx.self", silent=true)
            val vm = if(vmself.isEmpty) vx else vmself
            val wmself = c.typecheck(q"$wx.self", silent=true)
            val wm = if(wmself.isEmpty) wx else wmself

            // Crucial part: ensure that v is never used as the qualifier
            val vUsed = wm.find(vValDef.symbol == _.symbol)
            if (vUsed.isDefined) {
              c.info(vUsed.get.pos, s"Not lifting, because ${vValDef.symbol} is used on rhs", true)
              Some(TupleLiftingTransformer.super.transform(tree))
            }

            else {

              val transformedExpr = transform(expr)

              //              val vt: Type = vm.tpe.typeArgs.head
              //       val wt: Type = wm.tpe.typeArgs.head

              val vc = oldClosure
              val wc = oldInnerClosure

              val vt = vc.vparams.head.tpt.tpe
              val wt = wc.vparams.head.tpt.tpe

              // tupleLift[V,W]((wm,vm))
              //              val newQual = q"$lifter.tupleLift[$vt,$wt](($vm,$wm))" //
              // val newQual = q"($vm,$wm).tupled" //
              val blah = internal.reificationSupport.freshTermName("blah$")

              //


              val newQual = q"_root_.cats.Semigroupal.tuple2($vm, $wm)($semigroupal, $invariant)"

              // Function parameter of type (V,W) for new closure
              val tupleOfXYtt: Tree = tq"($vt,$wt)"
              val vwArgName = internal.reificationSupport.freshTermName("x$")
              val vwArgDef = ValDef(Modifiers(Flag.SYNTHETIC | Flag.PARAM), vwArgName, tupleOfXYtt, EmptyTree)

              // rhs of new closure, extracting v and w from tuple.
              // Note: at this point, the symbols for v and w are still owned by original closures.  We'll change
              // ownership below, after typing.
              val newExpr = Block(
                c.internal.valDef(vValDef.symbol, q"$vwArgName._1"),
                c.internal.valDef(wValDef.symbol, q"$vwArgName._2"),
                transformedExpr)

              val newClosure = Function(vwArgDef :: Nil, newExpr)

              // Assemble and type new combinator application:
              val ret = Apply(TypeApply(Select(newQual, innerMeth), innerMethTypeParam), newClosure :: Nil)
              val rett = c.typecheck(ret)

              // Extract the new closure after typing, so we can fix ownership problems.
              val Apply(_, newClosureTyped :: Nil) = rett
              // The new closure belongs to whoever owned the old closure.  At this point, they're
              // all called "anonfun$", but they're distinct.
              c.internal.setOwner(newClosureTyped.symbol, oldClosure.symbol.owner)
              // The new parameters belong to the new closure.
              c.internal.changeOwner(rett, vValDef.symbol.owner, newClosureTyped.symbol)
              c.internal.changeOwner(rett, wValDef.symbol.owner, newClosureTyped.symbol)
              c.info(tree.pos, s"Lifting to $rett", true)
              Some(rett)
            }

          // Boring map.  Possibly the inner of a nested map.  Actually, we don't bother to insist that
          // it's a map/flatMap, since that will be done in the previous case.
          case Apply(TypeApply(Select(wm, comb), _),
          Function(_ :: Nil, _) :: Nil) ⇒
            Some(TupleLiftingTransformer.super.transform(tree))

          case t ⇒
            None
        }
      }

      override def transform(tree: Tree): Tree = tree match {
        case PossiblyNestedMap(xformed) ⇒ xformed

        case _ ⇒
          super.transform(tree)
      }

    }

    val ret =  (new TupleLiftingTransformer).transform(expr)

    ret

  }

}

