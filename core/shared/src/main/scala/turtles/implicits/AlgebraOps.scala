/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.implicits

import turtles._

import cats._
import cats.implicits._

sealed class AlgebraOps[F[_], A](self: Algebra[F, A]) {
  def generalize[W[_]: Comonad](implicit F: Functor[F]): GAlgebra[W, F, A] =
    node => self(node.map(_.extract))

  def generalizeM[M[_]: Applicative](implicit F: Functor[F]): AlgebraM[M, F, A] =
    node => self(node).pure[M]

  def generalizeElgot[W[_]: Comonad]: ElgotAlgebra[W, F, A] =
    w => self(w.extract)

  def generalizeElgotM[W[_]: Comonad, M[_]: Monad]: ElgotAlgebraM[W, M, F, A] =
    w => self(w.extract).pure[M]
}
