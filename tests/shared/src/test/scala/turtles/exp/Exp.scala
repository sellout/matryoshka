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

package turtles.exp

import slamdata.Predef.{Eq => _, _}
import turtles._
import turtles.implicits._

import cats._
import cats.implicits._
import org.scalacheck._
import org.scalacheck.support.cats._

sealed abstract class Exp[A] extends Product with Serializable
case class Num[A](value: Int) extends Exp[A]
case class Mul[A](left: A, right: A) extends Exp[A]
case class Var[A](value: Symbol) extends Exp[A]
case class Lambda[A](param: Symbol, body: A) extends Exp[A]
case class Apply[A](func: A, arg: A) extends Exp[A]
case class Let[A](name: Symbol, value: A, inBody: A) extends Exp[A]

object Exp extends ExpInstances {
  implicit val arbSymbol = Arbitrary(Arbitrary.arbitrary[String].map(Symbol(_)))

  implicit val arbitrary: Delay[Arbitrary, Exp] = new Delay[Arbitrary, Exp] {
    def apply[α](arb: Arbitrary[α]): Arbitrary[Exp[α]] =
      Arbitrary(Gen.oneOf(
        Arbitrary.arbitrary[Int].map(Num[α](_)),
        (arb.arbitrary, arb.arbitrary).mapN(Mul(_, _)),
        Arbitrary.arbitrary[Symbol].map(Var[α](_)),
        (Arbitrary.arbitrary[Symbol], arb.arbitrary).mapN(Lambda(_, _)),
        (arb.arbitrary, arb.arbitrary).mapN(Apply(_, _)),
        (Arbitrary.arbitrary[Symbol], arb.arbitrary, arb.arbitrary).mapN(
          Let(_, _, _))))
  }

  implicit val cogen: Delay[Cogen, Exp] = new Delay[Cogen, Exp] {
    def apply[A](co: Cogen[A]) = Cogen((seed, value) => seed)
  }

  implicit val traverse: Traverse[Exp] = new Traverse[Exp] {

    def foldLeft[A, B](fa: Exp[A], b: B)(f: (B, A) => B) =
      fa match {
        case Num(_)           => b
        case Mul(left, right) => f(f(b, left), right)
        case Var(_)           => b
        case Lambda(_, body)  => f(b, body)
        case Apply(func, arg) => f(f(b, func), arg)
        case Let(_, v, i)     => f(f(b, v), i)
      }

    def foldRight[A, B](fa: Exp[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) =
      fa match {
        case Num(_)           => lb
        case Mul(left, right) => f(left, f(right, lb))
        case Var(_)           => lb
        case Lambda(_, b)     => f(b, lb)
        case Apply(func, arg) => f(func, f(arg, lb))
        case Let(_, v, i)     => f(v, f(i, lb))
      }

    def traverse[G[_], A, B](fa: Exp[A])(f: A => G[B])(implicit G: Applicative[G]): G[Exp[B]] = fa match {
      case Num(v)           => G.pure(Num(v))
      case Mul(left, right) => (f(left), f(right)).mapN(Mul(_, _))
      case Var(v)           => G.pure(Var(v))
      case Lambda(p, b)     => f(b).map(Lambda(p, _))
      case Apply(func, arg) => (f(func), f(arg)).mapN(Apply(_, _))
      case Let(n, v, i)     => (f(v), f(i)).mapN(Let(n, _, _))
    }
  }

  // NB: an unusual definition of equality, in that only the first 3 characters
  //     of variable names are significant. This is to distinguish it from `==`
  //     as well as from a derivable Eq.
  implicit val equal: Delay[Eq, Exp] = new Delay[Eq, Exp] {
    def apply[α](eq: Eq[α]) =
      Eq.instance[Exp[α]] {
        case (Num(v1), Num(v2))                 => v1 === v2
        case (Mul(a1, b1), Mul(a2, b2))         =>
          eq.eqv(a1, a2) && eq.eqv(b1, b2)
        case (Var(s1), Var(s2))                 =>
          s1.name.substring(0, 3 min s1.name.length) ==
          s2.name.substring(0, 3 min s2.name.length)
        case (Lambda(p1, a1), Lambda(p2, a2))   =>
          p1 == p2 && eq.eqv(a1, a2)
        case (Apply(f1, a1), Apply(f2, a2))     =>
          eq.eqv(f1, f2) && eq.eqv(a1, a2)
        case (Let(n1, v1, i1), Let(n2, v2, i2)) =>
          n1 == n2 && eq.eqv(v1, v2) && eq.eqv(i1, i2)
        case (_, _)                             => false
      }
  }

  // NB: Something like this currently needs to be defined for any Functor in
  //     order to get the generalize operations for the algebra.
  implicit def toExpAlgebraOps[A](a: Algebra[Exp, A]): AlgebraOps[Exp, A] =
    toAlgebraOps[Exp, A](a)

  implicit val show: Delay[Show, Exp] = new Delay[Show, Exp] {
    def apply[α](show: Show[α]) =
      Show.show {
        case Num(v)       => v.show
        case Mul(a, b)    =>
          "Mul(" + show.show(a) + ", " + show.show(b) + ")"
        case Var(s)       => "$" + s.name
        case Lambda(p, a) => "Lambda(" + p.name + ", " + show.show(a) + ")"
        case Apply(f, a)  =>
          "Apply(" + show.show(f) + ", " + show.show(a) + ")"
        case Let(n, v, i) =>
          "Let(" + n.name + ", " + show.show(v) + ", " + show.show(i) + ")"
      }
  }
}

trait ExpInstances {
  // implicit val alternative = new Alternative[Exp] {
  //   // Members declared in cats.Applicative
  //   def pure[A](x: A): turtles.exp.Exp[A] = ???
  //   // Members declared in cats.Apply
  //   def ap[A, B](ff: turtles.exp.Exp[A => B])(fa: turtles.exp.Exp[A]): turtles.exp.Exp[B] = ???
  //   // Members declared in cats.MonoidK
  //   def empty[A]: turtles.exp.Exp[A] = ???
  //   // Members declared in cats.SemigroupK
  //   def combineK[A](x: turtles.exp.Exp[A],y: turtles.exp.Exp[A]): turtles.exp.Exp[A] = ???
  //
  //   def unzip[A, B](f: Exp[(A, B)]) = (f.map(_._1), f.map(_._2))
  // }
}
