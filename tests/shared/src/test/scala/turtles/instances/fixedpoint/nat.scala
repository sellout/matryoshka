/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.instances.fixedpoint

import turtles._
import turtles.helpers.TurtlesSuite
import turtles.implicits._

class NatSpec extends TurtlesSuite {

  // FIXME: Need to restrict this to smaller numbers
  // checkAll("Nat ⇔ Int Prism", PrismTests(Nat.intPrism[Nat]))
  // checkAll("Conat", OrderTests[Conat].order)

  test("+ should sum values") {
    forAll { (a: Nat, b: Nat) =>
      val (ai, bi) = (a.cata(height), b.cata(height))
      (a + b).cata(height) should === (ai + bi)
    }
  }

}
