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

import slamdata.Predef.{Eq => _, _}
import turtles._
import turtles.data._
import turtles.exp.Exp
import turtles.exp2.Exp2
import turtles.helpers._
import turtles.scalacheck.arbitrary._
import turtles.scalacheck.cogen._

import cats._
import cats.functor._
import cats.implicits._
import cats.laws.discipline._
import org.specs2.mutable._
import org.scalacheck.support.cats._

class CoEnvSpec extends Specification with AlgebraChecks {
  "CoEnv" >> {
    // checkAll("CoEnv[String, Exp, Int]", EqTests[CoEnv[String, Exp, Int]].eq)
    checkAll("Eq[CoEnv[String, Exp, Int]]", SerializableTests.serializable(Eq[CoEnv[String, Exp, Int]]))

    checkAll("CoEnv[?, Exp, ?]", BitraverseTests[CoEnv[?, Exp, ?]].bitraverse)
    checkAll("Bitraverse[CoEnv[?, Exp, ?]]", SerializableTests.serializable(Bitraverse[CoEnv[?, Exp, ?]]))

    checkAll("CoEnv[Int, Exp, ?]", TraverseTests[CoEnv[Int, Exp, ?]].traverse)
    checkAll("Traverse[CoEnv[Int, Exp, ?]]", SerializableTests.serializable(Traverse[CoEnv[Int, Exp, ?]]))

    // NB: These test the low-prio Bi-functor/-foldable instances, so if `Exp2`
    //     gets a Traverse instance, these need to change.

    checkAll("CoEnv[?, Exp2, ?]", BifoldableTests[CoEnv[?, Exp2, ?]].bifoldable)
    checkAll("Bifoldable[CoEnv[?, Exp2, ?]]", SerializableTests.serializable(Bifoldable[CoEnv[?, Exp2, ?]]))

    checkAll("CoEnv[Int, Exp2, ?]", FoldableTests[CoEnv[Int, Exp2, ?]].foldable)
    checkAll("Foldable[CoEnv[Int, Exp2, ?]]", SerializableTests.serializable(Foldable[CoEnv[Int, Exp2, ?]]))

    checkAll("CoEnv[?, Exp2, ?]", BifunctorTests[CoEnv[?, Exp2, ?]].bifunctor)
    checkAll("Bifunctor[CoEnv[?, Exp2, ?]]", SerializableTests.serializable(Bifunctor[CoEnv[?, Exp2, ?]]))

    checkAll("CoEnv[Int, Exp2, ?]", FunctorTests[CoEnv[Int, Exp2, ?]].functor)
    checkAll("Functor[CoEnv[Int, Exp2, ?]]", SerializableTests.serializable(Functor[CoEnv[Int, Exp2, ?]]))

    // FIXME: These instances don’t fulfill the laws
    // monad.laws[CoEnv[String, Option, ?]].check(Test.Parameters.default)
    // monad.laws[CoEnv[String, NonEmptyList, ?]].check(Test.Parameters.default)

    checkAlgebraIsoLaws("CoEnv ⇔ Free", CoEnv.freeIso[Int, Exp])
  }
}
