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

import slamdata.Predef.{ Int, Some, None }
import turtles._
import turtles.data._
import turtles.derived._
import turtles.implicits._
import turtles.helpers.TurtlesSuite
import turtles.patterns.ListF
import cats.laws.discipline.FoldableTests

class ListSpec extends TurtlesSuite {

  // checkAll("List[Int]", EqTests[List[Int]].eqv)
  checkAll("List", FoldableTests[List].foldable[Int, Int])

  test("List.apply should be equivalent to scala.List.apply") {
    List(1, 2, 3, 4).cata(ListF.listIso.get) should === (scala.List(1, 2, 3, 4))
  }

  test("List.fill should be equivalent to scala.List.fill") {
    forAll { (n: Nat, v: Int) =>
      List.fill[scala.List[Int]](n)(v) should === (scala.List.fill(n.toInt)(v))
    }
  }

  test("List.length should count the number of elements") {
    forAll { (n: Nat, v: Int) =>
      List.fill[List[Int]](n)(v).length should === (n.toInt)
    }
  }

  test("List.headOption should return the first element") {
    List(1, 2, 3, 4).headOption should === (Some(1))
  }

  test("List.headOption should return None for an empty list") {
    List[Int]().headOption should === (None)
  }

  test("tailOption should return the remainder of the list") {
    List(1, 2, 3, 4).tailOption should === (List(2, 3, 4).some)
    List(1).tailOption should === (List[Int]().some)
  }

  test("tailOption should return None for an empty list ") {
    List[Int]().tailOption should === (None)
  }

}
