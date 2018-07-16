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
  implicit def steppableT: SteppableT[Mu] = new SteppableT[Mu] {
    def projectT[F[_]: Functor](t: Mu[F]) = lambek[Mu[F], F](t)(embedT)
    def embedT[F[_]: Functor](t: F[Mu[F]]) =
      Mu(new (Algebra[F, ?] ~> Id) {
        def apply[A](fa: Algebra[F, A]): A =
          fa(t.map(recursiveT.cataT(_)(fa)))
      })
  }

  implicit def recursiveT: RecursiveT[Mu] = new RecursiveT[Mu] {
    def cataT[F[_]: Functor, A](t: Mu[F])(f: Algebra[F, A]) = t.unMu(f)
  }

  implicit def corecursiveT: CorecursiveT[Mu] = new CorecursiveT[Mu] {
    def anaT[F[_]: Functor, A](a: A)(f: Coalgebra[F, A]) =
      hylo(a)(steppableT.embedT[F], f)
  }

  implicit def orderT: OrderT[Mu] = OrderT.steppableT

  implicit val showT: ShowT[Mu] = ShowT.recursiveT
}

abstract class MuInstancesʹ {
  implicit lazy val equalT: EqT[Mu] = EqT.steppableT
}
