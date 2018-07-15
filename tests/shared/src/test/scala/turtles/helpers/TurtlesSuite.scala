/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.helpers

import cats.{ Eq, Functor, Traverse }
import cats.tests.CatsSuite
import java.lang.String
import monocle.law.discipline.{ IsoTests, PrismTests }
import org.scalacheck.{ Arbitrary, Cogen }
import turtles._
import turtles.scalacheck.ArbitraryInstances
import turtles.scalacheck.CogenInstances

trait TurtlesSuite
  extends CatsSuite
     with ArbitraryInstances
     with CogenInstances {

  def checkFoldIsoLaws
    [T: Arbitrary: Eq, F[_]: Functor, A: Arbitrary: Eq: Cogen]
    (name: String, iso: AlgebraIso[F, A])
    (implicit T: Birecursive.Aux[T, F]) =
    checkAll(name + " Iso", IsoTests(foldIso[T, F, A](iso)))

  def checkFoldPrismLaws
    [T: Arbitrary: Eq, F[_]: Traverse, A: Arbitrary: Eq: Cogen]
    (name: String, prism: AlgebraPrism[F, A])
    (implicit T: Birecursive.Aux[T, F]) =
    checkAll(name + " Prism", PrismTests(foldPrism(prism)))

  def checkUnfoldPrismLaws
    [T: Arbitrary: Eq: Cogen, F[_]: Traverse, A: Arbitrary: Eq]
    (name: String, prism: CoalgebraPrism[F, A])
    (implicit T: Birecursive.Aux[T, F]) =
    checkAll(name + " Prism", PrismTests(unfoldPrism(prism)))

  def checkAlgebraIsoLaws[F[_], A: Arbitrary: Eq: Cogen]
    (name: String, iso: AlgebraIso[F, A])
    (implicit FA: Delay[Arbitrary, F], FE: Delay[Eq, F]) =
    checkAll(name + " Iso", IsoTests(iso))

  def checkAlgebraPrismLaws[F[_], A: Arbitrary: Eq: Cogen]
    (name: String, prism: AlgebraPrism[F, A])
    (implicit FA: Delay[Arbitrary, F], FE: Delay[Eq, F]) =
    checkAll(name + " Prism", PrismTests(prism))

  def checkCoalgebraPrismLaws[F[_], A: Arbitrary: Eq: Cogen]
    (name: String, prism: CoalgebraPrism[F, A])
    (implicit
      FA: Delay[Arbitrary, F],
      FE: Delay[Eq, F],
      FC: Delay[Cogen, F]) =
    checkAll(name + " Prism", PrismTests(prism))
}
