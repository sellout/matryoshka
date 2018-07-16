/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.data

import turtles._
import turtles.patterns.CoEnv

import cats._
import cats.free._
import cats.implicits._

trait FreeInstances {
  implicit def freeSteppable[F[_]: Functor, A]
      : Steppable.Aux[Free[F, A], CoEnv[A, F, ?]] =
    Steppable.fromAlgebraIso(
      _.run.fold(_.pure[Free[F, ?]], Free.liftF(_).flatten),
      t => CoEnv(t.resume.swap))

  implicit def freeBirecursive[F[_]: Functor, A]
      : Birecursive.Aux[Free[F, A], CoEnv[A, F, ?]] =
    Birecursive.withNativeRecursion

  implicit def freeEq[F[_]: Traverse](implicit F: Delay[Eq, F]):
      Delay[Eq, Free[F, ?]] =
    new Delay[Eq, Free[F, ?]] {
      def apply[A](eq: Eq[A]) = {
        implicit val coenvʹ: Delay[Eq, CoEnv[A, F, ?]] = CoEnv.equal(eq, F)

        Corecursive.corecursiveEq[Free[F, A], CoEnv[A, F, ?]]
      }
    }

  implicit def freeShow[F[_]: Functor](implicit F: Delay[Show, F]):
      Delay[Show, Free[F, ?]] =
    new Delay[Show, Free[F, ?]] {
      def apply[A](s: Show[A]) = {
        implicit val coenvʹ: Delay[Show, CoEnv[A, F, ?]] = CoEnv.show(s, F)

        Recursive.show[Free[F, A], CoEnv[A, F, ?]]
      }
    }
}

object free extends FreeInstances
