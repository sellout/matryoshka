/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

sealed abstract class ∘[F[_], G[_]] {
  type λ[A] = F[G[A]]
}
