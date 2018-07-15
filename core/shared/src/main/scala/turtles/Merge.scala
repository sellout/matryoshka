/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

import slamdata.Predef.{Eq => _, _}

import cats._
import cats.implicits._
import simulacrum._

/** Like `Cartesian`, but it can fail to merge, so it’s much more general.
  */
@typeclass trait Merge[F[_]] {
  // TODO[simulacrum#57]: `fa` should be lazy
  def merge[A, B](fa: F[A], fb: => F[B]): Option[F[(A, B)]]

  // TODO[simulacrum#57]: `fa` should be lazy
  def mergeWith[A, B, C, D](
    fa: F[A], fb: => F[B])(g: (A, B) => D)(
    implicit F: Functor[F]):
      Option[F[D]] =
    merge(fa, fb).map(_.map(g.tupled))
}

object Merge {
  implicit def fromTraverse[F[_]: Traverse](implicit E: Eq[F[Unit]]):
      Merge[F] =
    new Merge[F] {
      def merge[A, B](fa: F[A], fb: => F[B]): Option[F[(A, B)]] =
        if (fa.void === fb.void)
          fa.zipWithL(fb)((a, b) => b.map((a, _))).sequence
        else None
    }
}
