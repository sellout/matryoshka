/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

import cats._

/** This is a workaround for a certain use case (e.g.,
  * [[turtles.patterns.Diff]] and [[turtles.patterns.PotentialFailure]]).
  * Define an instance of this rather than [[Corecursive]] when possible.
  */
// NB: Not a `@typeclass` because we don’t want to inject these operations.
trait CorecursiveT[T[_[_]]] {
  def embedT[F[_]: Functor](t: F[T[F]]): T[F]

  def anaT[F[_]: Functor, A](a: A)(f: Coalgebra[F, A]): T[F] =
    hylo(a)(embedT[F], f)
}

object CorecursiveT {
  def apply[T[_[_]]](implicit instance: CorecursiveT[T]) = instance
}
