/*
 * Copyright 2014–2017 SlamData Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
