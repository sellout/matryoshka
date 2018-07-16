/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

import slamdata.Predef.{Eq => _, _}

/** An extractor to make it easier to pattern-match on arbitrary [[Steppable]]
  * structures.
  *
  * NB: This extractor is irrufutable and doesn’t break exhaustiveness checking.
  */
object Embed {
  def unapply[T, F[_]](obj: T)(implicit T: Steppable.Aux[T, F]): Some[F[T]] =
    Some(T.project(obj))
}
