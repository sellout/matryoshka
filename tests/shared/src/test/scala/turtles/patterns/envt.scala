/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
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
