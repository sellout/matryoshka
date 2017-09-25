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

import cats._

/** This is the simplest fixpoint type, implemented with general recursion.
  */
final case class Fix[F[_]](unFix: F[Fix[F]])

object Fix extends FixInstances

abstract class FixInstances extends FixInstancesʹ {
  implicit def birecursiveT: BirecursiveT[Fix] = new BirecursiveT[Fix] {
    def projectT[F[_]: Functor](t: Fix[F]) = t.unFix

    def embedT[F[_]: Functor](t: F[Fix[F]]) = Fix(t)
  }

  implicit def orderT: OrderT[Fix] = OrderT.recursiveT

  implicit val showT: ShowT[Fix] = ShowT.recursiveT
}

abstract class FixInstancesʹ {
  implicit val equalT: EqT[Fix] = EqT.recursiveT
}
