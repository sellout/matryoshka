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

import slamdata.Predef.{ String, Int }
import turtles.data._
import turtles.exp.Exp
import cats.kernel.laws.discipline.EqTests
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
