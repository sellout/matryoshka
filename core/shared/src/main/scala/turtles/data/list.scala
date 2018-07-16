/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.data

import slamdata.Predef.{Eq => _, _}
import turtles._
import turtles.derived._
import turtles.patterns._

trait ListInstances {
  implicit def listSteppable[A]: Steppable.Aux[List[A], ListF[A, ?]] =
    Steppable.fromAlgebraIso[List[A], ListF[A, ?]]({
      case ConsF(h, t) => h :: t
      case NilF()      => Nil
    }, {
      case h :: t => ConsF(h, t)
      case Nil    => NilF[A, List[A]]()
    })

  implicit def listRecursive[A]: Recursive.Aux[List[A], ListF[A, ?]] =
    Recursive.withNativeRecursion[List[A], ListF[A, ?]]

  implicit def listCorecursive[A]: Corecursive.Aux[List[A], ListF[A, ?]] =
    Corecursive.withNativeRecursion[List[A], ListF[A, ?]]
}

object list extends ListInstances
