/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

import cats._

import org.specs2.matcher._

trait CatsMatchers { outer =>

  /** Equality matcher with a [[cats.Eq]] typeclass */
  def eqv[T : Eq : Show](expected: T): Matcher[T] = new Matcher[T] {
    def apply[S <: T](actual: Expectable[S]): MatchResult[S] = {
      val actualT = actual.value.asInstanceOf[T]
      def test = Eq[T].eqv(expected, actualT)
      def koMessage = Show[T].show(actualT) + "!==" + Show[T].show(expected)
      def okMessage = Show[T].show(actualT) + "===" + Show[T].show(expected)
      Matcher.result(test, okMessage, koMessage, actual)
    }
  }

  class CatsBeHaveMatchers[T : Eq : Show](result: MatchResult[T]) {
    def eqv(t: T) = result.applyMatcher(outer.eqv[T](t)(Eq[T], Show[T]))
  }

  import scala.language.implicitConversions
  implicit def catsBeHaveMatcher[T : Eq : Show](result: MatchResult[T]) = new CatsBeHaveMatchers(result)
}

object CatsMatchers extends CatsMatchers
