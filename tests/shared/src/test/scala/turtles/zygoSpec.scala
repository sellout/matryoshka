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

import slamdata.Predef.{Eq => _, _}
// import turtles.data._
import turtles.exp._
// import turtles.helpers._
// import turtles.implicits._
// import turtles.runners._

// import cats._
// import cats.implicits._
// import org.scalacheck._, Prop._
import org.specs2.ScalaCheck
import org.specs2.mutable._
// import org.specs2.scalaz.ScalazMatchers

class ZygoSpecs extends Specification with ScalaCheck {

  implicit class EitherOps[B](b: B) {
    def right[A]: Either[A, B] = Right(b)
  }

  def extractFactors2: Coalgebra[Exp, Int] = { x =>
    def sqrt(x: Int): Int = scala.math.sqrt(x.toDouble).toInt

    if (x > 1 && sqrt(x) * sqrt(x) == x) Mul(sqrt(x), sqrt(x))
    else if (x > 2 && x % 2 == 0) Mul(2, x/2)
    else Num(x)
  }

  // "Recursive" >> {
  //   "zygo" >> {
  //     "eval and strings" in {
  //       testRec(
  //         mul(mul(num(0), num(0)), mul(num(2), num(5))),
  //         new RecRunner[Exp, String] {
  //           def run[T](implicit T: Recursive.Aux[T, Exp]) =
  //             _.zygo(eval, strings) must_== "0 (0), 0 (0) (0), 2 (2), 5 (5) (10)"
  //         })
  //     }
  //   }
  //
  //   "zygoM" >> {
  //     "behave like zygo" >> prop { (i: Int) =>
  //       val factors = i.ana[Fix[Exp]](extractFactors)
  //
  //       val a = factors.zygo(eval, strings)
  //       val b = factors.zygoM[String, Int, Int Either ?](
  //         eval.generalizeM[Int Either ?], strings(_).right)
  //
  //       a.right ?= b
  //     }
  //   }
  //
  //   "elgotZygo" >> {
  //     "eval and elgotStrings" in {
  //       testRec(
  //         mul(mul(num(0), num(0)), mul(num(2), num(5))),
  //         new RecRunner[Exp, Int Either String] {
  //           def run[T](implicit T: Recursive.Aux[T, Exp]) =
  //             _.elgotZygoM[String, Int, Int Either ?](
  //               eval.generalizeM[Int Either ?],
  //               elgotStrings(_).right
  //             ) must_== "((0 * 0 = 0) * (2 * 5 = 10) = 0)".right
  //         })
  //     }
  //   }
  //
  //   "elgotZygoM" >> {
  //     "behave like elgotZygo" >> prop { (i: Int) =>
  //       val factors = i.ana[Fix[Exp]](extractFactors2)
  //
  //       val a = factors.elgotZygo(eval, elgotStrings)
  //       val b = factors.elgotZygoM[String, Int, Int Either ?](
  //         eval.generalizeM[Int Either ?], elgotStrings(_).right)
  //
  //       a.right ?= b
  //     }
  //   }
  // }
}
