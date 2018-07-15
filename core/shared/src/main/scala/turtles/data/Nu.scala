/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.data

import turtles._

import cats._
import cats.implicits._

/** This is for coinductive (potentially infinite) recursive structures, models
  * the concept of “codata”, aka, the “greatest fixed point”.
  */
sealed abstract class Nu[F[_]] {
  type A
  val a: A
  val unNu: Coalgebra[F, A]
}

object Nu extends NuInstances {
  def apply[F[_], B](f: Coalgebra[F, B], b: B): Nu[F] =
    new Nu[F] {
      type A = B
      val a = b
      val unNu = f
    }
}

abstract class NuInstances extends NuInstancesʹ {
  implicit def birecursiveT: BirecursiveT[Nu] = new BirecursiveT[Nu] {
    def projectT[F[_]: Functor](t: Nu[F]) = t.unNu(t.a).map(Nu(t.unNu, _))

    // FIXME: ugh, shouldn’t have to redefine `colambek` in here?
    def embedT[F[_]: Functor](t: F[Nu[F]]) = anaT(t)(_.map(projectT[F]))
    override def anaT[F[_]: Functor, A](a: A)(f: A => F[A]) = Nu(f, a)
  }

  implicit def orderT: OrderT[Nu] = OrderT.recursiveT

  implicit val showT: ShowT[Nu] = ShowT.recursiveT
}

abstract class NuInstancesʹ {
  implicit lazy val equalT: EqT[Nu] = EqT.recursiveT
}
