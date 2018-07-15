/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
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
