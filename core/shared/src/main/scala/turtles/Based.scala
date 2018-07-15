/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

import simulacrum.typeclass

/** Provides a type describing the pattern functor of some {co}recursive type
  * `T`. For standard fixed-point types like [[turtles.data.Fix]],
  * `Based[Fix[F]]#Base` is simply `F`. However, directly recursive types
  * generally have a less obvious pattern functor. E.g., `Based[Cofree[F,
  * A]]#Base` is `EnvT[A, F, ?]`.
  */
@typeclass trait Based[T] {
  type Base[A]
}
