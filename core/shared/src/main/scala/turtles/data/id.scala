/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.data

import turtles._

import cats.data._

trait IdInstances {
  /** This is a single (low-priority) instance to provide folds/unfolds for all
    * non-recursive data types.
    */
  def idBirecursive[A]: Birecursive.Aux[A, Const[A, ?]] =
    Birecursive.fromAlgebraIso(_.getConst, Const(_))
}

object id extends IdInstances
