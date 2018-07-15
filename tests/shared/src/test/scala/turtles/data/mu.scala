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

class MuSpec extends TurtlesSuite {
  checkAll("Mu[Exp]", EqTests[Mu[Exp]].eqv)
  checkFoldIsoLaws[Mu[CoEnv[Int, Exp, ?]], CoEnv[Int, Exp, ?], Free[Exp, Int]]("Mu", CoEnv.freeIso)
}
