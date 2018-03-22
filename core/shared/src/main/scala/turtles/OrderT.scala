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
