/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.patterns

import org.scalacheck.{ Arbitrary, Gen }
import scala.Predef.{ implicitly => imp }
import slamdata.Predef.Int
import turtles.helpers.TurtlesSuite

class ListFSpec extends TurtlesSuite {
  // checkAll("ListF[String, Int]", EqTests[ListF[String, Int]].eqv)
  // checkAll("ListF", BitraverseTests[ListF].bitraverse)
  checkAlgebraIsoLaws("ListF ⇔ List", ListF.listIso[Int])(
    Arbitrary(Gen.listOf(Arbitrary.arbInt.arbitrary)), imp, imp, imp, imp
  )
}
