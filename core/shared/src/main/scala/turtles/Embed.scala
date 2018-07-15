/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

import slamdata.Predef.{Eq => _, _}
import turtles.implicits._

import cats._

/** An extractor to make it easier to pattern-match on arbitrary [[Recursive]]
  * structures.
  *
  * NB: This extractor is irrufutable and doesn’t break exhaustiveness checking.
  */
object Embed {
  def unapply[T, F[_]](obj: T)(implicit T: Recursive.Aux[T, F], F: Functor[F])
      : Some[F[T]] =
    Some(obj.project)
}
