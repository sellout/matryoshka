/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.patterns

import slamdata.Predef.{Eq => _, _}
import turtles._
import turtles.implicits._

import cats._
import cats.implicits._
import simulacrum._

@typeclass trait Diffable[F[_]] { self =>
  private implicit def selfʹ: Diffable[F] = self

  def diffImpl[T[_[_]]: BirecursiveT](l: T[F], r: T[F]):
      Option[DiffT[T, F]]

  /** Useful when a case class has a `List[A]` that isn’t the final `A`. This is
    * because the normal comparison just walks over the children of the functor,
    * so if the lists are different lengths, the default behavior will be
    * confusing.
    * Currently also useful when the only list _is_ the final parameter, because
    * it allows you to explicitly use `Similar` rather than `LocallyDifferent`.
    */
  def diffTraverse[T[_[_]]: BirecursiveT, G[_]: Traverse](
    left: G[T[F]], right: G[T[F]])(
    implicit FF: Functor[F], FoldF: Foldable[F], FM: Merge[F]):
      G[DiffT[T, F]] =
    if (left.toList.length < right.toList.length)
      left.zipWithR(right)((l, r) =>
        l.fold(
          Added[T, F, T[Diff[T, F, ?]]](r).embed)(
          _.paraMerga(r)(diff[T, F])))
    else
      left.zipWithL(right)((l, r) =>
        r.fold(
          Removed[T, F, T[Diff[T, F, ?]]](l).embed)(
          l.paraMerga(_)(diff[T, F])))

  // TODO: create something like Eq, but that overrides G[F[_]] (where G
  //       implements Traverse) to always be equal. This should allow us to
  //       distinguish between, say, two things containing a List[F[_]] that
  //       only differ on the length of the list. So we can make them `Similar`
  //       rather than `LocallyDifferent`.

  def localDiff[T[_[_]]: BirecursiveT](
    left: F[T[F]], right: F[T[F]])(
    implicit FT: Traverse[F], FM: Merge[F]):
      DiffT[T, F] =
    LocallyDifferent[T, F, T[Diff[T, F, ?]]](diffTraverse[T, F](left, right), right.void).embed
}
