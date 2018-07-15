/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.data

import turtles._

import cats._
import cats.implicits._

/** This is for inductive (finite) recursive structures, models the concept of
  * “data”, aka, the “least fixed point”.
  */
final case class Mu[F[_]](unMu: Algebra[F, ?] ~> Id)

object Mu extends MuInstances

abstract class MuInstances extends MuInstancesʹ {
  implicit def birecursiveT: BirecursiveT[Mu] = new BirecursiveT[Mu] {
    // FIXME: ugh, shouldn’t have to redefine `lambek` in here?
    def projectT[F[_]: Functor](t: Mu[F]) =
      cataT[F, F[Mu[F]]](t)(_.map(embedT[F]))
    override def cataT[F[_]: Functor, A](t: Mu[F])(f: Algebra[F, A]) = t.unMu(f)

    def embedT[F[_]: Functor](t: F[Mu[F]]) =
      Mu(new (Algebra[F, ?] ~> Id) {
        def apply[A](fa: Algebra[F, A]): A = fa(t.map(cataT(_)(fa)))
      })
  }

  implicit def orderT: OrderT[Mu] = OrderT.recursiveT

  implicit val showT: ShowT[Mu] = ShowT.recursiveT
}

abstract class MuInstancesʹ {
  implicit lazy val equalT: EqT[Mu] = EqT.recursiveT
}
