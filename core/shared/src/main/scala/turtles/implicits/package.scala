/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

import slamdata.Predef.{Eq => _, _}

import cats._
import cats.evidence._
import cats.free._

package object implicits
    extends Birecursive.ToBirecursiveOps
    with Merge.ToMergeOps
    with Recursive.ToRecursiveOps
    with Steppable.ToSteppableOps {

  implicit def toRecursive[T, F[_]](implicit T: Birecursive.Aux[T, F])
      : Recursive.Aux[T, F] =
    T.rec

  implicit def toCorecursive[T, F[_]](implicit T: Birecursive.Aux[T, F])
      : Corecursive.Aux[T, F] =
    T.corec

  implicit def toIdOps[A](a: A): IdOps[A] = new IdOps[A](a)

  implicit final class SteppableBaseOps[T, F[_], FF[_]](
    self: F[T])(
    implicit T: Steppable.Aux[T, FF], Sub: F[T] <~< FF[T]) {

    def embed(implicit F: Functor[FF]): T = T.embed(Sub(self))
  }

  implicit final class SteppableOps[T, F[_]]
    (self: T)(implicit T: Steppable.Aux[T, F]) {

    def transHylo[G[_]: Functor, U, H[_]: Functor]
      (φ: G[U] => H[U], ψ: F[T] => G[T])
      (implicit U: Steppable.Aux[U, H], BF: Functor[F])
        : U =
      turtles.transHylo(self)(φ, ψ)

    object transAna {
      def apply[U] = new PartiallyApplied[U]
      final class PartiallyApplied[U] {
        def apply[G[_]: Functor]
          (f: F[T] => G[T])
          (implicit
            US: Steppable.Aux[U, G],
            UC: Corecursive.Aux[U, G],
            BF: Functor[F])
            : U =
          UC.transAna(self)(f)
      }
    }

    def transGana[M[_]: Monad, U, G[_]: Functor]
      (k: DistributiveLaw[M, G], f: CoalgebraicGTransform[M, T, F, G])
      (implicit
        U: Corecursive.Aux[U, G],
        BF: Functor[F])
        : U =
      U.transGana(self)(k, f)

    def transApo[U, G[_]: Functor]
      (f: CoalgebraicGTransform[Either[U, ?], T, F, G])
      (implicit
        US: Steppable.Aux[U, G],
        UC: Corecursive.Aux[U, G],
        BF: Functor[F])
        : U =
      UC.transApo(self)(f)

    def transFutu[U, G[_]: Functor]
      (f: CoalgebraicGTransform[Free[G, ?], T, F, G])
      (implicit
        U: Corecursive.Aux[U, G],
        BF: Functor[F])
        : U =
      U.transFutu(self)(f)

    def transAnaM[M[_]: Monad, U, G[_]: Traverse]
      (f: TransformM[M, T, F, G])
      (implicit
        US: Steppable.Aux[U, G],
        UC: Corecursive.Aux[U, G],
        BF: Functor[F])
        : M[U] =
      UC.transAnaM(self)(f)
  }

  implicit final class BirecursiveOps[T, F[_], FF[_]](
    self: F[T])(
    implicit T: Birecursive.Aux[T, FF], Sub: F[T] <~< FF[T]) {

    def colambek(implicit TS: Steppable.Aux[T, FF], F: Functor[FF]): T =
      T.colambek(Sub(self))
  }

  implicit def toAlgebraOps[F[_], A](a: Algebra[F, A]): AlgebraOps[F, A] =
    new AlgebraOps[F, A](a)

  implicit def toCoalgebraOps[F[_], A](a: Coalgebra[F, A]): CoalgebraOps[F, A] =
    new CoalgebraOps[F, A](a)
}
