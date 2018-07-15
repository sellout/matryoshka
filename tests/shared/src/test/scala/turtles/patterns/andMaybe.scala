/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.patterns

import cats.laws.discipline.BitraverseTests
import slamdata.Predef.Int
import turtles.helpers.TurtlesSuite

class AndMaybeSpec extends TurtlesSuite {
  // checkAll("AndMaybe[String, Int]", EqTests[AndMaybe[String, Int]].eqv)
  checkAll("AndMaybe", BitraverseTests[AndMaybe].bitraverse[(Int, ?), Int, Int, Int, Int, Int, Int])
}
