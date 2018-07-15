/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.data

import slamdata.Predef.{Eq => _, _}
import turtles._

import cats.data._

trait EitherInstances {
  implicit def eitherBirecursive[A, B]
      : Birecursive.Aux[Either[A, B], Const[Either[A, B], ?]] =
    id.idBirecursive[Either[A, B]]
}

object either extends EitherInstances
