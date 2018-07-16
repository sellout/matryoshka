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
  def idSteppable[A]: Steppable.Aux[A, Const[A, ?]] =
    Steppable.fromAlgebraIso(_.getConst, Const(_))

  /** This is a single (low-priority) instance to provide folds/unfolds for all
    * non-recursive data types.
    */
  def idBirecursive[A](implicit A: Steppable.Aux[A, Const[A, ?]])
      : Birecursive.Aux[A, Const[A, ?]] =
    Birecursive.withNativeRecursion[A, Const[A, ?]]
}

object id extends IdInstances
