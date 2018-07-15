/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

import slamdata.Predef.{Eq => _, _}
import turtles.implicits._

import cats._
import simulacrum._

@typeclass trait OrderT[T[_[_]]] {
  def compare[F[_]: Functor](tf1: T[F], tf2: T[F])(implicit del: Delay[Order, F]):
      Int

  def orderT[F[_]: Functor](delay: Delay[Order, F]): Order[T[F]] =
    Order.from[T[F]](compare[F](_, _)(Functor[F], delay))
}

object OrderT {
  def recursiveT[T[_[_]]: RecursiveT]: OrderT[T] = new OrderT[T] {
    def compare[F[_]: Functor]
      (tf1: T[F], tf2: T[F])
      (implicit del: Delay[Order, F]) =
      del(orderT[F](del)).compare(tf1.project, tf2.project)
  }
}
