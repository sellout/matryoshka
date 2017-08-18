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

package turtles.example

import slamdata.Predef.{Eq => _, _}
import turtles._
import turtles.implicits._
import turtles.patterns._
import turtles.scalacheck.arbitrary._

import cats._
import cats.data.State
import cats.implicits._
// import cats.laws.discipline._
import org.scalacheck._
import org.scalacheck.support.cats._
import org.specs2.mutable._
import org.typelevel.discipline.specs2.mutable._

sealed abstract class Example[A]
final case class Empty[A]()                                   extends Example[A]
final case class NonRec[A](a: String, b: Int)                 extends Example[A]
final case class SemiRec[A](a: Int, b: A)                     extends Example[A]
final case class MultiRec[A](a: A, b: A)                      extends Example[A]
final case class OneList[A](l: List[A])                       extends Example[A]
final case class TwoLists[A](first: List[A], second: List[A]) extends Example[A]

object Example {
  implicit val traverse: Traverse[Example] = new Traverse[Example] {

    def foldLeft[A, B](fa: Example[A], b: B)(f: (B, A) => B): B =
    	traverse(fa)(a => State((b: B) => (f(b, a), ()))).runS(b).value

    def foldRight[A, B](fa: Example[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    	traverse(fa)(a => State((lb: Eval[B]) => (f(a, lb), ()))).runS(lb).value

    def traverse[G[_], A, B](fa: Example[A])(f: A => G[B])(
      implicit G: Applicative[G]):
        G[Example[B]] = fa match {
      case Empty()        => G.pure(Empty())
      case NonRec(a, b)   => G.pure(NonRec(a, b))
      case SemiRec(a, b)  => f(b).map(SemiRec(a, _))
      case MultiRec(a, b) => (f(a), f(b)).mapN(MultiRec(_, _))
      case OneList(a)     => a.traverse(f).map(OneList(_))
      case TwoLists(a, b) => (a.traverse(f), b.traverse(f)).mapN(TwoLists(_, _))
    }
  }

  implicit val equal: Delay[Eq, Example] = new Delay[Eq, Example] {
    def apply[α](eq: Eq[α]) = Eq.instance((a, b) => {
      implicit val ieq = eq
        (a, b) match {
        case (Empty(),          Empty())          => true
        case (NonRec(s1, i1),   NonRec(s2, i2))   => s1 === s2 && i1 === i2
        case (SemiRec(i1, a1),  SemiRec(i2, a2))  =>
          i1 === i2 && eq.eqv(a1, a2)
        case (MultiRec(a1, b1), MultiRec(a2, b2)) =>
          eq.eqv(a1, a2) && eq.eqv(b1, b2)
        case (OneList(l),       OneList(r))       => l === r
        case (TwoLists(l1, l2), TwoLists(r1, r2)) => l1 === r1 && l2 === r2
        case (_,                _)                => false
      }
    })
  }

  implicit val show: Delay[Show, Example] = new Delay[Show, Example] {
    def apply[α](s: Show[α]) = {
      implicit val is = s
      Show.show {
        case Empty()          => "Empty()"
        case NonRec(s2, i2)   =>
          "NonRec(" |+| s2.show |+| ", " |+| i2.show |+| ")"
        case SemiRec(i2, a2)   =>
          "SemiRec(" |+| i2.show |+| ", " |+| s.show(a2) |+| ")"
        case MultiRec(a2, b2) =>
          "MultiRec(" |+| s.show(a2) |+| ", " |+| s.show(b2) |+| ")"
        case OneList(r)       => "OneList(" |+| r.show |+| ")"
        case TwoLists(r1, r2) =>
          "TwoLists(" |+| r1.show |+| ", " |+| r2.show |+| ")"
      }
    }
  }

  implicit val diffable: Diffable[Example] = new Diffable[Example] {
    def diffImpl[T[_[_]]: BirecursiveT](l: T[Example], r: T[Example]):
        Option[DiffT[T, Example]] =
      (l.project, r.project) match {
        case (l @ Empty(),        r @ Empty())        => localDiff(l, r).some
        case (l @ NonRec(_, _),   r @ NonRec(_, _))   => localDiff(l, r).some
        case (l @ SemiRec(_, _),  r @ SemiRec(_, _))  => localDiff(l, r).some
        case (l @ MultiRec(_, _), r @ MultiRec(_, _)) => localDiff(l, r).some
        case (OneList(l),         OneList(r))         =>
          Similar[T, Example, T[Diff[T, Example, ?]]](OneList[DiffT[T, Example]](diffTraverse(l, r))).embed.some
        case (TwoLists(l1, l2),   TwoLists(r1, r2))   =>
          Similar[T, Example, T[Diff[T, Example, ?]]](TwoLists[DiffT[T, Example]](diffTraverse(l1, r1), diffTraverse(l2, r2))).embed.some
        case (_,                  _)                  => None
      }
  }

  implicit val arbitrary: Delay[Arbitrary, Example] =
    new Delay[Arbitrary, Example] {
      def apply[α](arb: Arbitrary[α]) =
        Arbitrary(Gen.sized(size =>
          Gen.oneOf(
            Empty[α]().pure[Gen],
            (Arbitrary.arbitrary[String], Arbitrary.arbitrary[Int]).mapN(
              NonRec[α](_, _)),
            (Arbitrary.arbitrary[Int], arb.arbitrary).mapN(SemiRec(_, _)),
            (arb.arbitrary, arb.arbitrary).mapN(MultiRec(_, _)),
            Gen.listOfN(size, arb.arbitrary).map(OneList(_)),
            (Gen.listOfN(size / 2, arb.arbitrary), Gen.listOfN(size / 2, arb.arbitrary)).mapN(
              TwoLists(_, _)))
        ))
    }
}

class ExampleSpec extends Specification with Discipline {
  // "Example" >> {
    // checkAll("Example[Int]", EqTests[Example[Int]].eqv)
    // checkAll("Example", TraverseTests[Example].traverse[Int, Int, Int, Int, (Int, ?), (Int, ?)])
  // }
}
