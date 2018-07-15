/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.patterns

import turtles.Delay

import cats._
import cats.implicits._

/** Generally similar to CoEnv (Free), this has an additional `success` case
  * that indicates there’s no failure down to the leaves.
  */
sealed abstract class PotentialFailure[T[_[_]], F[_], E, A]
final case class Success[T[_[_]], F[_], E, A] private[patterns](v: T[F])
    extends PotentialFailure[T, F, E, A]
/** Akin to Free.point */
final case class Failure[T[_[_]], F[_], E, A] private[patterns](e: E)
    extends PotentialFailure[T, F, E, A]
/** Akin to Free.roll */
final case class PartialFailure[T[_[_]], F[_], E, A] private[patterns](v: F[A])
    extends PotentialFailure[T, F, E, A]

object PotentialFailure {
  implicit def potentialFailureEq[T[_[_]], F[_], E: Eq](implicit T: Eq[T[F]], F: Delay[Eq, F]): Delay[Eq, PotentialFailure[T, F, E, ?]] =
    new Delay[Eq, PotentialFailure[T, F, E, ?]] {
      def apply[α](eq: Eq[α]) =
        Eq.instance {
          case (Success(v1),        Success(v2))        => T.eqv(v1, v2)
          case (Failure(e1),        Failure(e2))        => e1 === e2
          case (PartialFailure(v1), PartialFailure(v2)) => F(eq).eqv(v1, v2)
          case (_,                  _)                  => false
        }
    }


  // TODO: implement low-prio Bifunctor and Bifoldable with looser constraint on F
  implicit def bitraverse[T[_[_]], F[_]: Traverse]:
      Bitraverse[PotentialFailure[T, F, ?, ?]] =
    new Bitraverse[PotentialFailure[T, F, ?, ?]] {
      def bitraverse[G[_], A, B, C, D](
        fab: PotentialFailure[T, F, A, B])(
        f: A ⇒ G[C], g: B ⇒ G[D])(
        implicit G: Applicative[G]) =
        fab match {
          case Success(v)        => G.pure(Success(v))
          case Failure(e)        => f(e).map(Failure(_))
          case PartialFailure(v) => v.traverse(g).map(PartialFailure(_))
        }

      def bifoldLeft[A, B, C](
        fab: PotentialFailure[T, F, A, B], c: C)(
        f: (C, A) => C, g: (C, B) => C) =
        fab match {
          case Success(_)        => c
          case Failure(e)        => f(c, e)
          case PartialFailure(v) => v.foldLeft(c)(g)
        }

      def bifoldRight[A, B, C](
        fab: PotentialFailure[T, F, A, B], c: Eval[C])(
        f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]) =
        fab match {
          case Success(_)        => c
          case Failure(e)        => f(e, c)
          case PartialFailure(v) => v.foldRight(c)(g)
        }
    }
}
