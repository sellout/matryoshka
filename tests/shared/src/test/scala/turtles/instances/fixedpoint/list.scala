/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.instances.fixedpoint

import slamdata.Predef.{ Int, Some, None }
import turtles._
import turtles.data._
import turtles.derived._
import turtles.implicits._
import turtles.helpers.TurtlesSuite
import turtles.patterns.ListF
import cats.kernel.laws.discipline.EqTests
// import cats.laws.discipline.FoldableTests

class ListSpec extends TurtlesSuite {

  checkAll("List[Int]", EqTests[List[Int]].eqv)
  // checkAll("List", FoldableTests[List].foldable[Int, Int])

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
