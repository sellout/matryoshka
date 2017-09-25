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

package turtles.exp2

import slamdata.Predef.{Eq => _, _}
import turtles._

import cats._
import cats.implicits._
import org.scalacheck._

sealed abstract class Exp2[A]
case class Const[A]() extends Exp2[A]
case class Num2[A](value: Int) extends Exp2[A]
case class Single[A](a: A) extends Exp2[A]

object Exp2 {
  implicit val arbitrary: Delay[Arbitrary, Exp2] = new Delay[Arbitrary, Exp2] {
    def apply[α](arb: Arbitrary[α]): Arbitrary[Exp2[α]] =
      Arbitrary(Gen.oneOf(
        Gen.const(Const[α]),
        Arbitrary.arbitrary[Int].map(Num2[α](_)),
        arb.arbitrary.map(Single(_))))
  }

  // NB: This isn’t implicit in order to allow us to test our low-priority
  //     instances for CoEnv.
  val traverse: Traverse[Exp2] = new Traverse[Exp2] {
    def traverseImpl[G[_], A, B](
      fa: Exp2[A])(
      f: (A) ⇒ G[B])(
      implicit G: Applicative[G]) =
      fa match {
        case Const()   => G.pure(Const[B]())
        case Num2(v)   => G.pure(Num2[B](v))
        case Single(a) => f(a).map(Single(_))
      }
  }

  implicit val functor: Functor[Exp2] = traverse
  implicit val foldable: Foldable[Exp2] = traverse

  implicit val show: Delay[Show, Exp2] = new Delay[Show, Exp2] {
    def apply[α](show: Show[α]) =
      Show.show {
        case Const()   => "Const()"
        case Num2(v)   => "Num2(" + v.show + ")"
        case Single(a) => "Single(" + show.show(a) + ")"
      }
  }

  implicit val eq: Delay[Eq, Exp2] = new Delay[Eq, Exp2] {
    def apply[α](eq: Eq[α]) =
      Eq.instance[Exp2[α]] {
        case (Const(), Const())       => true
        case (Num2(v1), Num2(v2))     => v1 === v2
        case (Single(a1), Single(a2)) => eq.eqv(a1, a2)
        case _                        => false
      }
  }
}
