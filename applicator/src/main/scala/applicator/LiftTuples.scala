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
          //   tupleLift((v,w)).mapOrFlatMap { x$123 =>
          //     val v = x$123._1
          //     val w = x$123._2
          //     expr
          //   }
          case Apply(TypeApply(Select(vm, TermName("flatMap")), outerMethTypeParam),
                     (oldClosure@Function(vValDef :: Nil,
                              PossiblyNestedMap(Apply(TypeApply(Select(wm, innerMeth), innerMethTypeParam),
                                      (oldInnerClosure@Function(wValDef :: Nil, expr)) :: Nil)))) :: Nil)
            if (innerMeth == flatMapName || innerMeth == mapName) &&
              vm.tpe.typeConstructor == wm.tpe.typeConstructor ⇒

            val vmm = vm
            val wmm = wm

            //nval monadTypeConstructor = vmm.tpe.typeConstructor
            val tupleLifterObj = lifter

            if (tupleLifterObj.isEmpty) {
              c.info(tree.pos, s"Cannot find",true)
              Some(TupleLiftingTransformer.super.transform(tree))
            }

            else {
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
                val newQual = q"$tupleLifterObj.tupleLift[$vt,$wt](($vm,$wm))"

                // Parameter of type (V,W) for new closure
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
                // Extract the new closure again, so we can fix ownership problems.
                val Apply(_, newClosureTyped :: Nil) = rett
                c.internal.setOwner(newClosureTyped.symbol, oldClosure.symbol.owner)
                c.internal.changeOwner(rett, vValDef.symbol.owner, newClosureTyped.symbol)
                c.internal.changeOwner(rett, wValDef.symbol.owner, newClosureTyped.symbol)
                Some(rett)
              }
            }

          // Boring map.  Not nested.
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
