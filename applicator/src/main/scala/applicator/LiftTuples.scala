package applicator

import scala.collection.mutable
import scala.language.higherKinds
import scala.languageFeature.implicitConversions


object LiftTuples {

  import scala.language.experimental.macros
  import scala.reflect.macros.blackbox.Context

  def apply[M[_], T](expr: M[T])(implicit lifter: TupleLiftable[M]): M[T] = macro liftTuplesImpl[M, T]

  def liftTuplesImpl[M[_], T](c: Context)(expr: c.Tree)(lifter: c.Tree)(implicit M: c.WeakTypeTag[M[_]], T:c.WeakTypeTag[T]) = {
    import c.universe._

    val flatMapName = TermName("flatMap")
    val mapName = TermName("map")
    //val tupleLiftableClassName = "applicator.TupleLiftable"
    //val tupleLifterClassConstructor = typeOf[TupleLiftable[Option[_]]].typeSymbol // rootMirror.staticClass(tupleLiftableClassName)

    class TupleLiftingTransformer extends Transformer {

      private object PossiblyNestedMap {
        def unapply(tree: Tree): Option[Tree] = tree match {

          // Convert
          //   vm.flatMap { v => wm.mapOrFlatMap { w => expr } }
          // to
          //   tupleLift((vm,wm)).mapOrFlatMap { z$123 =>
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
          // tupleLift((vm,tupleLift((wm, xm)))).map { z$234 =>
          //    v = z$234._1
          //    z$123 = z$234._2
          //    {
          //      w = z$123._1
          //      x = z$123._2
          //      expr
          //    }
          // }
          case Apply(TypeApply(Select(vm, TermName("flatMap")), outerMethTypeParam),
          (oldClosure@Function(vValDef :: Nil,
          PossiblyNestedMap(Apply(TypeApply(Select(wm, innerMeth), innerMethTypeParam),
          (oldInnerClosure@Function(wValDef :: Nil, expr)) :: Nil)))) :: Nil)
            if (innerMeth == flatMapName || innerMeth == mapName) &&
              vm.tpe.typeConstructor == wm.tpe.typeConstructor ⇒

            // Crucial part: ensure that v is never used as the qualifier
            val vUsed = wm.find(vValDef.symbol == _.symbol)
            if (vUsed.isDefined) {
              c.info(vUsed.get.pos, s"Not lifting, because ${vValDef.symbol} is used on rhs", true)
              Some(TupleLiftingTransformer.super.transform(tree))
            }

            else {

              val transformedExpr = transform(expr)

              val vt: Type = vm.tpe.typeArgs.head
              val wt: Type = wm.tpe.typeArgs.head

              // tupleLift[V,W]((wm,vm))
              val newQual = q"$lifter.tupleLift[$vt,$wt](($vm,$wm))"

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
