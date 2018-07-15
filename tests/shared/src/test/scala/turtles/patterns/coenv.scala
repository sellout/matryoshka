/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.patterns

import cats.kernel.laws.discipline.EqTests
import slamdata.Predef.{ String, Int }
import turtles.data._
import turtles.exp.Exp
import turtles.helpers.TurtlesSuite

class CoEnvSpec extends TurtlesSuite {
  checkAll("CoEnv[String, Exp, Int]", EqTests[CoEnv[String, Exp, Int]].eqv)

  // checkAll("CoEnv[?, Exp, ?]", BitraverseTests[CoEnv[?, Exp, ?]].bitraverse)
  // checkAll("CoEnv[Int, Exp, ?]", TraverseTests[CoEnv[Int, Exp, ?]].traverse)

  // NB: These test the low-prio Bi-functor/-foldable instances, so if `Exp2`
  //     gets a Traverse instance, these need to change.

  // checkAll("CoEnv[?, Exp2, ?]", BifoldableTests[CoEnv[?, Exp2, ?]].bifoldable)
  // checkAll("CoEnv[Int, Exp2, ?]", FoldableTests[CoEnv[Int, Exp2, ?]].foldable)
  // checkAll("CoEnv[?, Exp2, ?]", BifunctorTests[CoEnv[?, Exp2, ?]].bifunctor)
  // checkAll("CoEnv[Int, Exp2, ?]", FunctorTests[CoEnv[Int, Exp2, ?]].functor)

  // FIXME: These instances don’t fulfill the laws
  // monad.laws[CoEnv[String, Option, ?]].check(Test.Parameters.default)
  // monad.laws[CoEnv[String, NonEmptyList, ?]].check(Test.Parameters.default)

  checkAlgebraIsoLaws("CoEnv ⇔ Free", CoEnv.freeIso[Int, Exp])
}
