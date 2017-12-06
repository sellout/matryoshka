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

import slamdata.Predef.Int
import turtles.data.cofree._
import turtles.exp.Exp
import turtles.helpers.TurtlesSuite

class EnvTSpec extends TurtlesSuite {
  // checkAll("EnvT[String, Exp, Int]", EqTests[EnvT[String, Exp, Int]].eqv)
  // checkAll("EnvT[String, NonEmptyList, ?]", ComonadTests[EnvT[String, NonEmptyList, ?]].comonad)
  checkAlgebraIsoLaws("EnvT ⇔ Cofree", EnvT.cofreeIso[Int, Exp])
}
