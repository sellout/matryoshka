/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.data

import slamdata.Predef.{Eq => _, _}

import turtles._
import turtles.derived._
import turtles.patterns._

import cats.data._

trait NonEmptyListInstances {
  implicit def nelSteppable[A]
      : Steppable.Aux[NonEmptyList[A], AndMaybe[A, ?]] =
    Steppable.fromAlgebraIso({
      case Indeed(a, bs) => NonEmptyList(a, bs.toList)
      case Only(a)       => NonEmptyList(a, Nil)
    }, {
      case NonEmptyList(a, b :: cs) => Indeed(a, NonEmptyList(b, cs))
      case NonEmptyList(a, Nil)     => Only(a)
    })

  implicit def nelBirecursive[A]
      : Birecursive.Aux[NonEmptyList[A], AndMaybe[A, ?]] =
    Birecursive.withNativeRecursion
}

object nel extends NonEmptyListInstances
