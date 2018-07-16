/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.data

import turtles._

import cats._

/** This is the simplest fixpoint type, implemented with general recursion.
  */
final case class Fix[F[_]](unFix: F[Fix[F]])

object Fix extends FixInstances

abstract class FixInstances extends FixInstancesʹ {
  implicit def steppableT: SteppableT[Fix] = new SteppableT[Fix] {
    def embedT[F[_]: Functor](t: F[Fix[F]]) = Fix(t)
    def projectT[F[_]: Functor](t: Fix[F]) = t.unFix
  }

  implicit def birecursiveT: BirecursiveT[Fix] = new BirecursiveT[Fix] {
    def cataT[F[_]: Functor, A](t: Fix[F])(f: Algebra[F, A]) =
      hylo(t)(f, steppableT.projectT[F])

    def anaT[F[_]: Functor, A](a: A)(f: Coalgebra[F, A]) =
      hylo(a)(steppableT.embedT[F], f)
  }

  implicit def orderT: OrderT[Fix] = OrderT.steppableT

  implicit val showT: ShowT[Fix] = ShowT.recursiveT
}

abstract class FixInstancesʹ {
  implicit lazy val equalT: EqT[Fix] = EqT.steppableT
}
