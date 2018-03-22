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

package turtles.patterns

// import slamdata.Predef.{Eq => _, _}
// import turtles._
// import turtles.data.Mu
// import turtles.example._
// import turtles.exp._
import turtles.helpers._
// import turtles.implicits._
// import turtles.scalacheck.arbitrary._

// import scala.Predef.implicitly

// import cats._
// import cats.implicits._
// import cats.laws.discipline._
// import org.scalacheck._
// import org.specs2.ScalaCheck
// import org.specs2.mutable._
// import org.typelevel.discipline.specs2.mutable._

class DiffSpec extends TurtlesSuite {

  // implicit val blah: cats.Functor[Gen] = scala.Predef.??? : cats.Functor[Gen]
  // implicit val blax: cats.Semigroupal[Gen] = scala.Predef.??? : cats.Semigroupal[Gen]

  // implicit def diffArbitrary[T[_[_]], F[_]](
  //   implicit T: Arbitrary[T[F]], F: Delay[Arbitrary, F]):
  //     Delay[Arbitrary, Diff[T, F, ?]] =
  //   new Delay[Arbitrary, Diff[T, F, ?]] {
  //     def apply[α](arb: Arbitrary[α]) =
  //       Arbitrary(Gen.oneOf(
  //         T.arbitrary.map(Same[T, F, α](_)),
  //         F(arb).arbitrary.map(Similar[T, F, α](_)),
  //         (T.arbitrary, T.arbitrary).mapN(Different[T, F, α](_, _)),
  //         (F(arb).arbitrary, F[Unit](implicitly).arbitrary).mapN(
  //           LocallyDifferent[T, F, α](_, _)),
  //         F(arb).arbitrary.map(Inserted[T, F, α](_)),
  //         F(arb).arbitrary.map(Deleted[T, F, α](_)),
  //         T.arbitrary.map(Added[T, F, α](_)),
  //         T.arbitrary.map(Removed[T, F, α](_))))
  //   }

  // checkAll("Diff[Mu, Exp, Int]", EqTests[Diff[Mu, Exp, Int]].eqv)
  // checkAll("Diff[Mu, Exp, ?]", TraverseTests[Diff[Mu, Exp, ?]].traverse)

  // "diff" should {
  //   "find non-recursive differences" in {
  //     NonRec[Mu[Example]]("a", 1).embed.paraMerga(NonRec[Mu[Example]]("b", 1).embed)(diff) must
  //     equal(
  //       LocallyDifferent[Mu, Example, Mu[Diff[Mu, Example, ?]]](
  //         NonRec[Mu[Diff[Mu, Example, ?]]]("a", 1),
  //         NonRec[Unit]("b", 1)).embed)
  //   }
  //
  //   "create `Removed` for shorter list" in {
  //     OneList(List(Empty[Mu[Example]]().embed, Empty[Mu[Example]]().embed)).embed.paraMerga(
  //       OneList(List(NonRec[Mu[Example]]("x", 3).embed)).embed)(diff) must
  //     equal(
  //       Similar[Mu, Example, Mu[Diff[Mu, Example, ?]]](
  //         OneList(List(
  //           Different[Mu, Example, Mu[Diff[Mu, Example, ?]]](Empty[Mu[Example]]().embed, NonRec[Mu[Example]]("x", 3).embed).embed,
  //           Removed[Mu, Example, Mu[Diff[Mu, Example, ?]]](Empty[Mu[Example]]().embed).embed))).embed)
  //   }
  //
  //   "create `Added` for longer list" in {
  //     OneList(List(NonRec[Mu[Example]]("x", 3).embed)).embed.paraMerga(
  //       OneList(List(Empty[Mu[Example]]().embed, Empty[Mu[Example]]().embed)).embed)(diff) must
  //     equal(
  //       Similar[Mu, Example, Mu[Diff[Mu, Example, ?]]](OneList(List(
  //         Different[Mu, Example, Mu[Diff[Mu, Example, ?]]](NonRec[Mu[Example]]("x", 3).embed, Empty[Mu[Example]]().embed).embed,
  //         Added[Mu, Example, Mu[Diff[Mu, Example, ?]]](Empty[Mu[Example]]().embed).embed))).embed)
  //   }
  //
  //   "choose “simplest” diff" in {
  //     OneList(List(Empty[Mu[Example]]().embed)).embed.paraMerga(
  //       OneList(List(NonRec[Mu[Example]]("x", 3).embed, Empty[Mu[Example]]().embed)).embed)(diff) must
  //     equal(
  //       Similar[Mu, Example, Mu[Diff[Mu, Example, ?]]](OneList(List(
  //         Added[Mu, Example, Mu[Diff[Mu, Example, ?]]](NonRec[Mu[Example]]("x", 3).embed).embed,
  //         Same[Mu, Example, Mu[Diff[Mu, Example, ?]]](Empty[Mu[Example]]().embed).embed))).embed)
  //   }.pendingUntilFixed("can’t just walk lists from start to finish")
  //
  //   "create `Removed` and `Added` for mixed lists" in {
  //     TwoLists(List(Empty[Mu[Example]]().embed, Empty[Mu[Example]]().embed), List(Empty[Mu[Example]]().embed)).embed.paraMerga(
  //       TwoLists(
  //         List(NonRec[Mu[Example]]("x", 3).embed),
  //         List(Empty[Mu[Example]]().embed, Empty[Mu[Example]]().embed, Empty[Mu[Example]]().embed)).embed)(diff) must
  //     equal(
  //       Similar[Mu, Example, Mu[Diff[Mu, Example, ?]]](TwoLists(
  //         List(
  //           Different[Mu, Example, Mu[Diff[Mu, Example, ?]]](Empty[Mu[Example]]().embed, NonRec[Mu[Example]]("x", 3).embed).embed,
  //           Removed[Mu, Example, Mu[Diff[Mu, Example, ?]]](Empty[Mu[Example]]().embed).embed),
  //         List(
  //           Same[Mu, Example, Mu[Diff[Mu, Example, ?]]](Empty[Mu[Example]]().embed).embed,
  //           Added[Mu, Example, Mu[Diff[Mu, Example, ?]]](Empty[Mu[Example]]().embed).embed,
  //           Added[Mu, Example, Mu[Diff[Mu, Example, ?]]](Empty[Mu[Example]]().embed).embed))).embed)
  //   }
  // }
  //
  // "shows" should {
  //   // NB: The `Different` case should look like this, but `drawTree` doesn’t
  //   //     properly handle `Show`s with newlines.
  //   //
  //   //     +- vvvvvvvvv left  vvvvvvvvv
  //   //     |  Empty()
  //   //     |  =========================
  //   //     |  NonRec("x", 3)
  //   //     |  ^^^^^^^^^ right ^^^^^^^^^
  //   //     |
  //   "print a tree diff" in {
  //     val left =
  //       TwoLists(
  //         List(
  //           NonRec[Mu[Example]]("foo", 3).embed,
  //           SemiRec(2,
  //             SemiRec(8,
  //               NonRec[Mu[Example]]("meh", 6).embed).embed).embed,
  //           Empty[Mu[Example]]().embed,
  //           Empty[Mu[Example]]().embed),
  //         List(Empty[Mu[Example]]().embed)).embed
  //
  //     val right =
  //       TwoLists(
  //         List(
  //           NonRec[Mu[Example]]("bar", 3).embed,
  //           SemiRec(10,
  //             SemiRec(8,
  //               NonRec[Mu[Example]]("meh", 7).embed).embed).embed,
  //           NonRec[Mu[Example]]("x", 3).embed),
  //         List(
  //           Empty[Mu[Example]]().embed,
  //           Empty[Mu[Example]]().embed,
  //           Empty[Mu[Example]]().embed)).embed
  //
  //     // TODO: Eliminate this type annotation
  //     ((left paraMerga right)(diff): Mu[Diff[Mu, Example, ?]]).cata(toTree).drawTree must
  //       equal("""TwoLists([(),(),(),()], [(),(),()])
  //               ||
  //               |+- NonRec("foo", 3) <=/=> NonRec("bar", 3)
  //               ||
  //               |+- SemiRec(2, ()) <=/=> SemiRec(10, ())
  //               ||  |
  //               ||  `- SemiRec(8, ())
  //               ||     |
  //               ||     `- NonRec("meh", 6) <=/=> NonRec("meh", 7)
  //               ||
  //               |+- vvvvvvvvv left  vvvvvvvvv
  //               |Empty()
  //               |=========================
  //               |NonRec("x", 3)
  //               |^^^^^^^^^ right ^^^^^^^^^
  //               ||
  //               |+- <--- Empty()
  //               ||
  //               |+- ...
  //               ||
  //               |+- +++> Empty()
  //               ||
  //               |`- +++> Empty()
  //               |""".stripMargin)
  //   }
  // }
}
