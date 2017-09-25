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

import slamdata.Predef.{Eq => _, _}
import turtles._
import turtles.exp._
import turtles.helpers._
import turtles.patterns.CoEnv
import turtles.scalacheck.arbitrary._
import turtles.scalacheck.cogen._

import cats._
import cats.free._
import cats.implicits._
import cats.laws.discipline._
import org.specs2.mutable._

class FixSpec extends Specification with AlgebraChecks {

  "Fix" >> {
    // checkAll("Fix[Exp]", EqTests[Fix[Exp]].eq)
    checkAll("Eq[Fix[Exp]]", SerializableTests.serializable(Eq[Fix[Exp]]))

    checkFoldIsoLaws[Fix[CoEnv[Int, Exp, ?]], CoEnv[Int, Exp, ?], Free[Exp, Int]]("Fix", CoEnv.freeIso)
  }
}
