/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

import turtles.implicits._

import java.lang.String

import cats._
import cats.implicits._
import simulacrum._

@typeclass trait ShowT[T[_[_]]] {
  def show[F[_]: Functor](tf: T[F])(implicit del: Delay[Show, F]): String

  def showT[F[_]: Functor](delay: Delay[Show, F]): Show[T[F]] =
    Show.show[T[F]](show[F](_)(Functor[F], delay))
}

object ShowT {
  def recursiveT[T[_[_]]: RecursiveT]: ShowT[T] = new ShowT[T] {
    override def show[F[_]: Functor](tf: T[F])(implicit del: Delay[Show, F]) =
      tf.cata(del(Show[String]).show)
  }
}
