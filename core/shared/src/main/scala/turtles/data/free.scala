/*
 * Copyright 2014–2017 SlamData Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package turtles.data

import turtles._
import turtles.patterns.CoEnv

import cats._
import cats.free._
import cats.implicits._

trait FreeInstances {
  implicit def freeBirecursive[F[_]: Functor, A]
      : Birecursive.Aux[Free[F, A], CoEnv[A, F, ?]] =
    Birecursive.algebraIso(
      _.run.fold(_.pure[Free[F, ?]], Free.liftF(_).flatten),
      t => CoEnv(t.resume.swap))

  implicit def freeEq[F[_]: Traverse](implicit F: Delay[Eq, F]):
      Delay[Eq, Free[F, ?]] =
    new Delay[Eq, Free[F, ?]] {
      def apply[A](eq: Eq[A]) = {
        implicit val coenvʹ: Delay[Eq, CoEnv[A, F, ?]] = CoEnv.equal(eq, F)

        Birecursive.biEq[Free[F, A], CoEnv[A, F, ?]]
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
