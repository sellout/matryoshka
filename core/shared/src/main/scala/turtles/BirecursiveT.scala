/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

/** This is a workaround for a certain use case (e.g.,
  * [[turtles.patterns.Diff]] and [[turtles.patterns.PotentialFailure]]).
  * Define an instance of this rather than [[Recursive]] and [[Corecursive]]
  * when possible.
  */
// NB: Not a `@typeclass` because we don’t want to inject these operations.
trait BirecursiveT[T[_[_]]] extends RecursiveT[T] with CorecursiveT[T]

object BirecursiveT {
  def apply[T[_[_]]](implicit instance: BirecursiveT[T]) = instance
}
