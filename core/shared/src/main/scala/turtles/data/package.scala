/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

import turtles.implicits._

import cats._
import cats.free._
import cats.implicits._

/** This packages contains fixed-point operators as well as instances of
  * recursion schemes for various extant data types.
  *
  * The reason these are relegated to their own package is because, in general,
  * you should eschew using them directly, but rather rely on the type class
  * constraints, and only require specific types at the boundaries.
  */
package object data
    extends CofreeInstances
    with EitherInstances
    with FreeInstances
    with IdInstances
    with ListInstances
    with NonEmptyListInstances
    with OptionInstances {

  /** NB: Since Cofree carries the functor, the resulting algebra is a cata, not
    *     a para.
    *
    * @group algtrans
    */
  def attributePara[T, F[_]: Functor, A]
    (f: GAlgebra[(T, ?), F, A])
    (implicit TS: Steppable.Aux[T, F], TC: Corecursive.Aux[T, F])
      : Algebra[F, Cofree[F, A]] =
    fa => Cofree(f(fa.map(x => (x.cata[T](_.lower.embed), x.head))), Now(fa))
}
