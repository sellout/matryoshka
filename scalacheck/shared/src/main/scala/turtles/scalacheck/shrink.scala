/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.scalacheck

import turtles._
import turtles.data._
import turtles.implicits._

import cats._
import cats.implicits._
import org.scalacheck._

trait ShrinkInstancesʹ {
  implicit def delayShrink[F[_], A](implicit A: Shrink[A], F: Delay[Shrink, F]): Shrink[F[A]] =
    F(A)
}

trait ShrinkInstances extends ShrinkInstancesʹ {
  /** An instance for [[turtles.Steppable]] types where the [[Base]] is
    * [[cats.Foldable]].
    */
  def foldableShrink[T, F[_]: Functor: Foldable]
    (implicit T: Steppable.Aux[T, F])
      : Shrink[T] =
    Shrink(_.project.toList.toStream)

  /** An instance for [[turtles.Steppable]] types where the [[Base]] has a
    * [[scalacheck.Shrink]] instance.
    */
  def shrinkShrink[T, F[_]: Functor]
    (implicit T: Steppable.Aux[T, F], F: Shrink[F[T]])
      : Shrink[T] =
    Shrink(t => F.shrink(t.project).map(_.embed))

  /** An instance for [[turtles.Steppable]] types where the [[Base]] has
    * both [[cats.Foldable]] and [[scalacheck.Shrink]] instances.
    */
  def shrinkFoldableShrink[T, F[_]: Functor: Foldable]
    (implicit T: Steppable.Aux[T, F], F: Shrink[F[T]])
      : Shrink[T] =
    Shrink(t => shrinkShrink[T, F].shrink(t) |+| foldableShrink[T, F].shrink(t))

  implicit def fixShrink[F[_]: Functor: Foldable](implicit F: Shrink[F[Nu[F]]])
      : Shrink[Fix[F]] =
    shrinkFoldableShrink[Fix[F], F]

  implicit def muShrink[F[_]: Functor: Foldable](implicit F: Shrink[F[Nu[F]]])
      : Shrink[Mu[F]] =
    shrinkFoldableShrink[Mu[F], F]

  implicit def nuShrink[F[_]: Functor: Foldable](implicit F: Shrink[F[Nu[F]]])
      : Shrink[Nu[F]] =
    shrinkFoldableShrink[Nu[F], F]
}

package object shrink extends ShrinkInstances
