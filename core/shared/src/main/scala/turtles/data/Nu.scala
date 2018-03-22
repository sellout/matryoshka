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
import cats.implicits._

/** This is for coinductive (potentially infinite) recursive structures, models
  * the concept of “codata”, aka, the “greatest fixed point”.
  */
sealed abstract class Nu[F[_]] {
  type A
  val a: A
  val unNu: Coalgebra[F, A]
}

object Nu extends NuInstances {
  def apply[F[_], B](f: Coalgebra[F, B], b: B): Nu[F] =
    new Nu[F] {
      type A = B
      val a = b
      val unNu = f
    }
}

abstract class NuInstances extends NuInstancesʹ {
  implicit def birecursiveT: BirecursiveT[Nu] = new BirecursiveT[Nu] {
    def projectT[F[_]: Functor](t: Nu[F]) = t.unNu(t.a).map(Nu(t.unNu, _))

    // FIXME: ugh, shouldn’t have to redefine `colambek` in here?
    def embedT[F[_]: Functor](t: F[Nu[F]]) = anaT(t)(_.map(projectT[F]))
    override def anaT[F[_]: Functor, A](a: A)(f: A => F[A]) = Nu(f, a)
  }

  implicit def orderT: OrderT[Nu] = OrderT.recursiveT

  implicit val showT: ShowT[Nu] = ShowT.recursiveT
}

abstract class NuInstancesʹ {
  implicit lazy val equalT: EqT[Nu] = EqT.recursiveT
}
