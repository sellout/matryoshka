/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

import turtles.data._
import turtles.implicits._

import cats._
import org.scalatest._

package object runners {

  def testRec[F[_]](t: Fix[F], r: RecRunner[F])(implicit F: Functor[F])
      : Assertion = {
    r.run[Fix[F]].apply(t)
    r.run[Mu[F]].apply(t.convertTo[Mu[F]])
    r.run[Nu[F]].apply(t.convertTo[Nu[F]])
  }

  def testCorec[M[_], F[_]: Functor, A]
    (a: A, r: CorecRunner[M, F, A])
    (implicit Eq0: Delay[Eq, F], S0: Delay[Show, F])
      : Assertion = {
    r.run[Fix[F]].apply(a)
    r.run[Mu[F]].apply(a)
    r.run[Nu[F]].apply(a)
  }
}
