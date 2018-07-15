/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.data

import cats.free.Free
import cats.kernel.laws.discipline.EqTests
import slamdata.Predef.Int
import turtles.exp.Exp
import turtles.helpers.TurtlesSuite
import turtles.patterns.CoEnv

class FixSpec extends TurtlesSuite {
  checkAll("Fix[Exp]", EqTests[Fix[Exp]].eqv)
  checkFoldIsoLaws[Fix[CoEnv[Int, Exp, ?]], CoEnv[Int, Exp, ?], Free[Exp, Int]]("Fix", CoEnv.freeIso)
}
