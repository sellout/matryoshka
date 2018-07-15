/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

import slamdata.Predef.{Eq => _, _}
import turtles.implicits._

import cats._
import simulacrum._

@typeclass trait EqT[T[_[_]]] {
  def eqv[F[_]: Functor](tf1: T[F], tf2: T[F])(implicit del: Delay[Eq, F]):
      Boolean

  def eqT[F[_]: Functor](delay: Delay[Eq, F]): Eq[T[F]] =
    Eq.instance[T[F]](eqv[F](_, _)(Functor[F], delay))
}

object EqT {
  def recursiveT[T[_[_]]: RecursiveT]: EqT[T] = new EqT[T] {
    def eqv[F[_]: Functor]
      (tf1: T[F], tf2: T[F])
      (implicit del: Delay[Eq, F]) =
      del(eqT[F](del)).eqv(tf1.project, tf2.project)
  }
}
