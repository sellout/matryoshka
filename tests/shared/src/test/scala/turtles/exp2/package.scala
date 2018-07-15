/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

import slamdata.Predef.{Eq => _, _}
import turtles.data.Fix

package object exp2 {
  def const = Fix[Exp2](Const())
  def num2(v: Int) = Fix[Exp2](Num2(v))
  def single(a: Fix[Exp2]) = Fix[Exp2](Single(a))
}
