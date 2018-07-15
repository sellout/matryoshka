/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.implicits

import turtles._

import cats._
import cats.implicits._

sealed class CoalgebraOps[F[_], A](self: Coalgebra[F, A]) {
  def generalize[N[_]: Applicative](implicit F: Functor[F])
      : GCoalgebra[N, F, A] =
    self(_).map(_.pure[N])

  def generalizeM[M[_]: Applicative]: CoalgebraM[M, F, A] = self(_).pure[M]

  def generalizeElgot[N[_]: Applicative]: ElgotCoalgebra[N, F, A] =
    self(_).pure[N]
}
