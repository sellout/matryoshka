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

import turtles.data._
import turtles.implicits._

import cats._
import org.scalatest._

package object runners {

  def testRec[F[_]](t: Fix[F], r: RecRunner[F])(implicit F: Functor[F])
      : Assertion = {
    r.run[Fix[F]].apply(t)
    r.run[Mu[F]].apply(t.convertTo[Mu[F]])
    r.run[Nu[F]].apply(t.convertTo[Nu[F]])
  }

  def testBirec[F[_]](t: Fix[F], r: BirecRunner[F])(implicit F: Functor[F])
      : Assertion = {
    r.run[Fix[F]].apply(t)
    r.run[Mu[F]].apply(t.convertTo[Mu[F]])
    r.run[Nu[F]].apply(t.convertTo[Nu[F]])
  }

  def testCorec[M[_], F[_]: Functor, A]
    (a: A, r: CorecRunner[M, F, A])
    (implicit Eq0: Delay[Eq, F], S0: Delay[Show, F])
      : Assertion = {
    r.run[Fix[F]].apply(a)
    r.run[Mu[F]].apply(a)
    r.run[Nu[F]].apply(a)
  }
}
