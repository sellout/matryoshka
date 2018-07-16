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
    extends Merge.ToMergeOps
    with Recursive.ToRecursiveOps
    with Steppable.ToSteppableOps {

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

    def transAnaT(f: T => T)(implicit TC: Corecursive.Aux[T, F], BF: Functor[F])
        : T =
      TC.transAnaT(self)(f)

    def transApoT
      (f: T => Either[T, T])
      (implicit TC: Corecursive.Aux[T, F], BF: Functor[F])
        : T =
      TC.transApoT(self)(f)

    def transAnaTM[M[_]: Monad]
      (f: T => M[T])
      (implicit TC: Corecursive.Aux[T, F], BF: Traverse[F])
        : M[T] =
      TC.transAnaTM(self)(f)

    def topDownCata[A]
      (a: A)
      (f: (A, T) => (A, T))
      (implicit TC: Corecursive.Aux[T, F], BF: Functor[F])
        : T =
      TC.topDownCata[A](self, a)(f)

    def topDownCataM[M[_]: Monad, A]
      (a: A)
      (f: (A, T) => M[(A, T)])
      (implicit TC: Corecursive.Aux[T, F], BT: Traverse[F])
        : M[T] =
      TC.topDownCataM[M, A](self, a)(f)
  }

  implicit def toAlgebraOps[F[_], A](a: Algebra[F, A]): AlgebraOps[F, A] =
    new AlgebraOps[F, A](a)

  implicit def toCoalgebraOps[F[_], A](a: Coalgebra[F, A]): CoalgebraOps[F, A] =
    new CoalgebraOps[F, A](a)
}
