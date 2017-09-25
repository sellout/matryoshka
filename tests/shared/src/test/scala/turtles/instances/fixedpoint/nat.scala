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

package turtles.instances.fixedpoint

import turtles._, CatsMatchers._
import turtles.data._
import turtles.implicits._
import turtles.scalacheck.arbitrary._

import cats._
import cats.implicits._
import cats.laws.discipline._
// import monocle.law.discipline._
import org.specs2.mutable._
import org.typelevel.discipline.specs2.mutable._

class NatSpec extends Specification with Discipline {
  // FIXME: Need to restrict this to smaller numbers
  // checkAll("Nat ⇔ Int Prism", PrismTests(Nat.intPrism[Nat]))

  "Nat" >> {
    // checkAll("Conat", OrderTests[Conat].order)
    checkAll("Order[Conat]", SerializableTests.serializable(Order[Conat]))
  }

  "+" should {
    "sum values" >> prop { (a: Nat, b: Nat) =>
      val (ai, bi) = (a.cata(height), b.cata(height))
      (a + b).cata(height) must be eqv(ai + bi)
    }
  }
}
