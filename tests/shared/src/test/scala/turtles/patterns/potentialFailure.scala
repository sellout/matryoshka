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

package turtles.patterns

import org.scalacheck.{ Arbitrary, Gen }
import turtles.Delay
import turtles.helpers.TurtlesSuite

class PotentialFailureSpec extends TurtlesSuite {

  implicit def potentialFailureArbitrary[T[_[_]], F[_], E: Arbitrary](
    implicit T: Arbitrary[T[F]], F: Delay[Arbitrary, F]):
      Delay[Arbitrary, PotentialFailure[T, F, E, ?]] =
    new Delay[Arbitrary, PotentialFailure[T, F, E, ?]] {
      def apply[α](arb: Arbitrary[α]) =
        Arbitrary(Gen.oneOf(
          T.arbitrary.map(Success[T, F, E, α](_)),
          Arbitrary.arbitrary[E].map(Failure[T, F, E, α](_)),
          F(arb).arbitrary.map(PartialFailure[T, F, E, α](_))))
    }

  // checkAll("PotentialFailure[Fix, Exp, String, Int]", EqTests[PotentialFailure[Fix, Exp, String, Int]].eqv)
  // checkAll("PotentialFailure[Fix, Exp, ?, ?]", BitraverseTests[PotentialFailure[Fix, Exp, ?, ?]].bitraverse)
  // checkAll("PotentialFailure[Fix, Exp, String, ?]", TraverseTests[PotentialFailure[Fix, Exp, String, ?]].traverse)

}
