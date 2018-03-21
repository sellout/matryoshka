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
import turtles.data._
import turtles.exp._
import turtles.exp2._
import turtles.helpers._
import turtles.implicits._
import turtles.instances.fixedpoint.{Nat, Partial, partialMonad, PartialOps, RecursiveOptionOps}
import turtles.patterns._
import turtles.runners._
import turtles.scalacheck.arbitrary._
import turtles.scalacheck.cogen._

import cats.{Apply => _, _}
import cats.arrow._
import cats.data._
import cats.free._
import cats.implicits._
// import cats.laws.discipline._
import org.scalacheck._
import org.specs2.mutable._
import org.typelevel.discipline.specs2.mutable._

import scala.util.{ Either => \/, Left => -\/, Right => \/- }

// class ExpSpec extends Specification with Discipline {
//   // NB: These are just a sanity check that the data structure created for the
//   //     tests is lawful.
//   "Exp" >> {
//     // checkAll("Exp", EqTests[Exp[Int]].eqv)
//     checkAll("Exp", TraverseTests[Exp].traverse)
//   }
// }
//
// class Exp2Spec extends Specification with Discipline {
//   // NB: These are just a sanity check that the data structure created for the
//   //     tests is lawful.
//   "Exp2" >> {
//     // checkAll("Exp2", EqTests[Exp2[Int]].eqv)
//     checkAll("Exp2", FunctorTests[Exp2].functor)
//     checkAll("Exp2", FoldableTests[Exp2].foldable)
//   }
// }

class TurtlesSpecs extends Specification with Discipline with AlgebraChecks {
  val example1ƒ: Exp[Option[Int]] => Option[Int] = {
    case Num(v)           => v.some
    case Mul(left, right) => (left, right).mapN(_ * _)
    case Var(v)           => None
    case Lambda(_, b)     => b
    case Apply(func, arg) => None
    case Let(_, _, i)     => i
  }

  val addOneOptƒ = λ[Exp ~> (Option ∘ Exp)#λ] {
    case Num(n) => Num(n+1).some
    case _      => None
  }

  def addOneƒ[T]: Exp[T] => Exp[T] =
    orOriginal(addOneOptƒ(_))

  val addOneOptExpExp2ƒ = λ[Exp ~> (Option ∘ Exp2)#λ] {
    case Num(n) => Num2(n+1).some
    case _      => None
  }

  def addOneExpExp2ƒ[T]: Exp[T] => Exp2[T] =
    orDefault[Exp[T], Exp2[T]](exp2.Const())(addOneOptExpExp2ƒ(_))

  def addOneOptExp2Expƒ = λ[Exp2 ~> (Option ∘ Exp)#λ] {
    case Num2(n) => Num(n+1).some
    case _       => None
  }

  def addOneExp2Expƒ[T]: Exp2[T] => Exp[T] =
    orDefault[Exp2[T], Exp[T]](Num(0))(addOneOptExp2Expƒ(_))

  def simplifyƒ[T](implicit T: Recursive.Aux[T, Exp])
      : Exp[T] => Option[Exp[T]] = {
    case Mul(a, b) => (a.project, b.project) match {
      case (Num(0), Num(_)) => Num(0).some
      case (Num(1), Num(n)) => Num(n).some
      case (Num(_), Num(0)) => Num(0).some
      case (Num(n), Num(1)) => Num(n).some
      case (_,      _)      => None
    }
    case _         => None
  }

  def addOneOrSimplifyƒ[T](implicit T: Recursive.Aux[T, Exp])
      : Exp[T] => Partial[Exp[T]] = {
    case t @ Num(_)    => addOneƒ(t).pure[Partial]
    case t @ Mul(_, _) => repeatedly[Partial[Exp[T]]](simplifyƒ[T]).apply(t)
    case t             => t.pure[Partial]
  }

  def extractLambdaƒ[T[_[_]]: RecursiveT]: Exp[(T[Exp], T[Exp2])] => Exp2[T[Exp2]] = {
    case Lambda(_, (exp, exp2)) => exp.project match {
      case Num(a) => Num2(a)
      case _      => Single(exp2)
    }
    case _                      => exp2.Const[T[Exp2]]
  }

  val MinusThree: Exp ~> Exp =
    new (Exp ~> Exp) {
      def apply[A](exp: Exp[A]): Exp[A] = exp match {
        case Num(x) => Num(x-3)
        case t      => t
      }
    }

  // NB: This is better done with cata, but we fake it here
  def partialEval[T]
    (t: Exp[Cofree[Exp, T]])
    (implicit T: Birecursive.Aux[T, Exp])
      : T =
    t match {
      case Mul(x, y) => (x.head.project, y.head.project) match {
        case (Num(a), Num(b)) => Num[T](a * b).embed
        case _                => t.map(_.head).embed
      }
      case _ => t.map(_.head).embed
    }

  checkAlgebraIsoLaws("birec", Birecursive.iso[Mu[Exp], Exp])
  checkAlgebraIsoLaws("lambek", Birecursive.lambekIso[Mu[Exp], Exp])

  "Recursive" >> {
    "isLeaf" >> {
      "be true for simple literal" in {
        num(1).isLeaf must beTrue
        num(1).convertTo[Mu[Exp]].isLeaf must beTrue
        num(1).convertTo[Nu[Exp]].isLeaf must beTrue
      }

      "be false for expression" in {
        mul(num(1), num(2)).isLeaf must beFalse
        mul(num(1), num(2)).convertTo[Mu[Exp]].isLeaf must beFalse
        mul(num(1), num(2)).convertTo[Nu[Exp]].isLeaf must beFalse
      }
    }

    "children" >> {
      "be empty for simple literal" in {
        num(1).children[List[Fix[Exp]]] must be empty;
        num(1).convertTo[Mu[Exp]].children[List[Mu[Exp]]] must be empty;
        num(1).convertTo[Nu[Exp]].children[List[Nu[Exp]]] must be empty
      }

      "contain sub-expressions" in {
        mul(num(1), num(2)).children[List[Fix[Exp]]] should === (List(num(1), num(2)))
        // mul(num(1), num(2)).convertTo[Mu[Exp]].children[List[Mu[Exp]]] should
        //   === (List(num(1), num(2)).map(_.convertTo[Mu[Exp]]))
        // mul(num(1), num(2)).convertTo[Nu[Exp]].children[List[Nu[Exp]]] should
        //   === (List(num(1), num(2)).map(_.convertTo[Nu[Exp]]))
      }
    }

    "universe" >> {
      "be one for simple literal" in {
        num(1).elgotPara(universe) should === (NonEmptyList.of(num(1)))
        // num(1).convertTo[Mu[Exp]].elgotPara(universe) should
        //   === (NonEmptyList.of(num(1)).map(_.convertTo[Mu[Exp]]))
        // num(1).convertTo[Nu[Exp]].elgotPara(universe) should
        //   === (NonEmptyList.of(num(1)).map(_.convertTo[Nu[Exp]]))
      }

      "contain root and sub-expressions" in {
        mul(num(1), num(2)).elgotPara(universe) should
          === (NonEmptyList.of(mul(num(1), num(2)), num(1), num(2)))
        // mul(num(1), num(2)).convertTo[Mu[Exp]].elgotPara(universe) should
        //   === (NonEmptyList.of(mul(num(1), num(2)), num(1), num(2)).map(_.convertTo[Mu[Exp]]))
        // mul(num(1), num(2)).convertTo[Nu[Exp]].elgotPara(universe) should
        //   === (NonEmptyList.of(mul(num(1), num(2)), num(1), num(2)).map(_.convertTo[Nu[Exp]]))
      }
    }

    "transCata" >> {
      "change simple literal" in {
        testRec(
          num(1),
          new RecRunner[Exp, Fix[Exp]] {
            def run[T](implicit T: Recursive.Aux[T, Exp]) =
              _.transCata[Fix[Exp]](addOneƒ) should === (num(2))
          })
      }

      "change sub-expressions" in {
        testRec(
          mul(num(1), num(2)),
          new RecRunner[Exp, Fix[Exp]] {
            def run[T](implicit T: Recursive.Aux[T, Exp]) =
              _.transCata[Fix[Exp]](addOneƒ) should === (mul(num(2), num(3)))
          })
      }

      "be bottom-up" in {
        (mul(num(0), num(1)).transCataM[Partial, Fix[Exp], Exp](addOneOrSimplifyƒ).unsafePerformSync should === (num(2))) and
        (mul(num(1), num(2)).transCataM[Partial, Fix[Exp], Exp](addOneOrSimplifyƒ).unsafePerformSync should === (mul(num(2), num(3))))
      }
    }

    "transAna" >> {
      "change simple literal" in {
        testCorec(
          num(1),
          new CorecRunner[Id, Exp, Fix[Exp]] {
            def run[T: Eq: Show](implicit T: Birecursive.Aux[T, Exp]) =
              _.transAna[T](addOneƒ) should === (num(2).convertTo[T])
          })
      }

      "change sub-expressions" in {
        testCorec(
          mul(num(1), num(2)),
          new CorecRunner[Id, Exp, Fix[Exp]] {
            def run[T: Eq: Show](implicit T: Birecursive.Aux[T, Exp]) =
              _.transAna[T](addOneƒ) should === (mul(num(2), num(3)).convertTo[T])
          })
      }

      "be top-down" in {
        mul(num(0), num(1)).transAnaM[Partial, Fix[Exp], Exp](addOneOrSimplifyƒ).unsafePerformSync should === (num(0))
        mul(num(1), num(2)).transAnaM[Partial, Fix[Exp], Exp](addOneOrSimplifyƒ).unsafePerformSync should === (num(2))
      }
    }

    "prepro" >> {
      "multiply original with identity ~>" in {
        mul(num(1), mul(num(12), num(8)))
          .prepro(FunctionK.id[Exp], example1ƒ) should
          === (96.some)
      }

      "apply ~> repeatedly" in {
        mul(num(1), mul(num(12), num(8))).prepro(MinusThree, example1ƒ) should
          === (-24.some)
      }
    }

    "gprepro" >> {
      "multiply original with identity ~>" in {
        lam('meh, mul(vari('meh), mul(num(10), num(8))))
          .gprepro[Cofree[Exp, ?], Fix[Exp]](
            distHisto, FunctionK.id[Exp], partialEval[Fix[Exp]]) should
          === (lam('meh, mul(vari('meh), num(80))))
      }

      "apply ~> repeatedly" in {
        lam('meh, mul(vari('meh), mul(num(13), num(8))))
          .gprepro[Cofree[Exp, ?], Fix[Exp]](
            distHisto, MinusThree, partialEval[Fix[Exp]]) should
          === (lam('meh, mul(vari('meh), num(-4))))
      }
    }

    "transPrepro" >> {
      "change literal with identity ~>" in {
        testBirec(
          num(1),
          new BirecRunner[Exp, Fix[Exp]] {
            def run[T](implicit T: Birecursive.Aux[T, Exp]) =
              _.transPrepro[Fix[Exp]](FunctionK.id, addOneƒ) should
                === (num(2))
          })
      }

      "apply ~> in original space" in {
        testBirec(
          mul(num(1), mul(num(12), num(8))),
          new BirecRunner[Exp, Fix[Exp]] {
            def run[T](implicit T: Birecursive.Aux[T, Exp]) =
              _.transPrepro[Fix[Exp]](MinusThree, addOneƒ) should
                === (mul(num(-1), mul(num(7), num(3))))
          })
      }

      "apply ~> with change of space" in {
        testBirec(
          num(1),
          new BirecRunner[Exp, Fix[Exp2]] {
            def run[T](implicit T: Birecursive.Aux[T, Exp]) =
              _.transPrepro[Fix[Exp2]](MinusThree, addOneExpExp2ƒ) should === (num2(2))
          })
      }
    }

    "transPostpro" >> {
      "change literal with identity ~>" in {
        testCorec(
          num(1),
          new CorecRunner[Id, Exp, Fix[Exp]] {
            def run[T: Eq: Show](implicit T: Birecursive.Aux[T, Exp]) =
              _.transPostpro[T](FunctionK.id, addOneƒ) should
                === (num(2).convertTo[T])
          })
      }

      "apply ~> in original space" in {
        testCorec(
          mul(num(1), mul(num(12), num(8))),
          new CorecRunner[Id, Exp, Fix[Exp]] {
            def run[T: Eq: Show](implicit T: Birecursive.Aux[T, Exp]) =
              _.transPostpro[T](MinusThree, addOneƒ) should
                === (mul(num(-1), mul(num(7), num(3))).convertTo[T])
          })
      }

      "apply ~> with change of space" in {
        testCorec(
          num2(1),
          new CorecRunner[Id, Exp, Fix[Exp2]] {
            def run[T: Eq: Show](implicit T: Birecursive.Aux[T, Exp]) =
              _.transPostpro[T](MinusThree, addOneExp2Expƒ) should
                === (num(2).convertTo[T])
          })
      }
    }

    "transPara" >> {
      "project basic exp" in {
        lam('sym, num(3)).transPara[Fix[Exp2]](extractLambdaƒ[Fix]) should === (num2(3))
      }

      "project basic exp recursively" in {
        lam('sym, mul(num(5), num(7))).transPara[Fix[Exp2]](extractLambdaƒ[Fix]) should
          === (single(const))
      }
    }

    "foldMap" >> {
      "fold stuff" in {
        mul(num(0), num(1)).foldMap(_ :: Nil) should === (mul(num(0), num(1)) :: num(0) :: num(1) :: Nil)
      }
    }

    val findConstants: Exp[List[Int]] => List[Int] = {
      case Num(x) => x :: Nil
      case t      => t.fold
    }

    "cata" >> {
      "evaluate simple expr" in {
        testRec(
          mul(num(1), mul(num(2), num(3))),
          new RecRunner[Exp, Int] {
            def run[T](implicit T: Recursive.Aux[T, Exp]) =
              _.cata(eval) should === (6)
          })
      }

      "find all constants" in {
        testRec(
          mul(num(0), num(1)),
          new RecRunner[Exp, List[Int]] {
            def run[T](implicit T: Recursive.Aux[T, Exp]) =
              _.cata(findConstants) should === (List(0, 1))
          })
      }

      "produce correct annotations for 5 * 2" in {
        testRec(
          mul(num(5), num(2)),
          new RecRunner[Exp, Option[Int]] {
            def run[T](implicit T: Recursive.Aux[T, Exp]) =
              _.cata(example1ƒ) must beSome(10)
          })
      }
    }

    "zipAlgebras" >> {
      "both eval and find all constants" in {
        testRec(
          mul(num(5), num(2)),
          new RecRunner[Exp, (Int, List[Int])] {
            def run[T](implicit T: Recursive.Aux[T, Exp]) =
              _.cata(AlgebraSemigroupal[Exp].product(eval, findConstants)) should
                === ((10, List(5, 2)))
          })
      }
    }

    "generalize" >> {
      "behave like cata" in {
        testRec(
          mul(num(1), mul(num(2), num(3))),
          new RecRunner[Exp, Int] {
            def run[T](implicit T: Recursive.Aux[T, Exp]) = t =>
              t.para(eval.generalize[(T, ?)]) should === (t.cata(eval))
          })
      }
    }

    "coelgot" >> {
      "behave like cofCata <<< attributeAna" >> prop { (i: Int) =>
        i.coelgot(eval.generalizeElgot[(Int, ?)], extractFactors) should === (
          i.ana[Cofree[Exp, Int]](attributeCoalgebra(extractFactors)).cata(liftT(eval.generalizeElgot[(Int, ?)])))
      }
    }

    "elgot" >> {
      "behave like interpCata <<< freeAna" >> prop { (i: Int) =>
        i.elgot(eval, extractFactors.generalizeElgot[Int \/ ?]) should === (
          i.ana[Free[Exp, Int]](runT(extractFactors.generalizeElgot[Int \/ ?])).cata(patterns.recover(eval)))
      }
    }

    "generalizeElgot" >> {
      "behave like cata on an algebra" ! prop { (i: Int) =>
        val x = i.ana[Fix[Exp]](extractFactors).cata(eval)
        i.coelgot(eval.generalizeElgot[(Int, ?)], extractFactors) should === (x)
      }

      "behave like ana on an coalgebra" ! prop { (i: Int) =>
        val x = i.ana[Fix[Exp]](extractFactors).cata(eval)
        i.elgot(eval, extractFactors.generalizeElgot[Int \/ ?]) should === (x)
      }
    }

    def extractFactors: Coalgebra[Exp, Int] = x =>
      if (x > 2 && x % 2 == 0) Mul(2, x/2)
      else Num(x)


    val freeVars: Exp[List[Symbol]] => List[Symbol] = {
      case Var(name) => name :: Nil
      case Lambda(param, body) => body.filterNot(_ == param)
      case t         => t.fold
    }

    val freeVarsNumber: ElgotAlgebra[(List[Symbol], ?), Exp, Int] = {
      case (vars, _) => vars.size
    }

    val generateVars: Int => Exp[Int] = n => Var(Symbol("x" + ((n * 13) % n)))

    val generateTerm: ElgotCoalgebra[Int \/ ?, Exp, (Int, Int)] = {
      case (n, 0)     if n <= 0      => Num(42).asRight
      case (n, bound) if n <= 0      => bound.asLeft
      case (n, bound) if (n % 2) === 0 => Lambda(Symbol("x" + bound), (n - 1, bound + 1)).asRight
      case (n, bound)                => \/-(Mul((n / 2, bound), ((n / 2) - 1, bound)))
    }

    "elgotCata" >> {

      "find no free vars in closed term" in {
        val t = lam('sym, mul(num(5), vari('sym)))
        t.elgotCata(distZygo(freeVars), freeVarsNumber) should === (0)
      }

      "find some free vars in open term" in {
        val t1 = lam('bound, mul(vari('open), vari('bound)))
        val t2 = lam('x, mul(vari('y), lam('y, mul(vari('open), mul(vari('y), vari('x))))))
        t1.elgotCata(distZygo(freeVars), freeVarsNumber) should === (1)
        t2.elgotCata(distZygo(freeVars), freeVarsNumber) should === (2)
      }
    }

    "elgotAna" >> {
      "generate closed terms" in {
        (3, 0).elgotAna[Fix[Exp]].apply(distGApo(generateVars), generateTerm) should ===  (
          mul(mul(num(42), num(42)), num(42))
        )

        (4, 0).elgotAna[Fix[Exp]].apply(distGApo(generateVars), generateTerm) should ===  (
          lam('x0, mul(mul(vari('x0), vari('x0)), vari('x0)))
        )

        (5, 0).elgotAna[Fix[Exp]].apply(distGApo(generateVars), generateTerm) should ===  (
          mul(lam('x0, mul(vari('x0), vari('x0))), mul(num(42), num(42)))
        )
      }
      "generate open terms" in {
        (3, 1).elgotAna[Fix[Exp]].apply(distGApo(generateVars), generateTerm) should ===  (
          mul(mul(vari('x0), vari('x0)), vari('x0))
        )
      }
    }

    "elgotHylo" >> {
      val freeVarsDistr = distZygo[Exp, List[Symbol]](freeVars)
      val varGenDistr = distGApo[Exp, Int](generateVars)
      def closed(n: Int, bound: Int) =
        elgotHylo((n, bound))(freeVarsDistr, varGenDistr, freeVarsNumber, generateTerm)

      "assert that closed terms don't have free vars" in {
        for (i <- (0 to 10)) {
          closed(i, 0) should ===  (0)
        }
        closed(11, 0) should ===  (0)
      }
    }

    "generalizeCoalgebra" >> {
      "behave like ana" ! prop { (i: Int) =>
        i.apo[Fix[Exp]](extractFactors.generalize[Fix[Exp] \/ ?]) should
          === (i.ana[Fix[Exp]](extractFactors))
        // i.apo[Mu[Exp]](extractFactors.generalize[Mu[Exp] \/ ?]) should
        //   === (i.ana[Mu[Exp]](extractFactors))
        // i.apo[Nu[Exp]](extractFactors.generalize[Nu[Exp] \/ ?]) should
        //   === (i.ana[Nu[Exp]](extractFactors))
      }
    }

    "topDownCata" >> {
      def subst[T]
        (vars: Map[Symbol, T], t: T)
        (implicit T: Recursive.Aux[T, Exp])
          : (Map[Symbol, T], T) = t.project match {
        case Let(sym, value, body) => (vars + ((sym, value)), body)
        case Var(sym)              => (vars, vars.get(sym).getOrElse(t))
        case _                     => (vars, t)
      }

      "bind vars" in {
        val v = let('x, num(1), mul(num(0), vari('x)))
        v.topDownCata(Map.empty[Symbol, Fix[Exp]])(subst) should
          === (mul(num(0), num(1)))
        // v.convertTo[Mu[Exp]].topDownCata(Map.empty[Symbol, Mu[Exp]])(subst) should
        //   === (mul(num(0), num(1)).convertTo[Mu[Exp]])
        // v.convertTo[Nu[Exp]].topDownCata(Map.empty[Symbol, Nu[Exp]])(subst) should
        //   === (mul(num(0), num(1)).convertTo[Nu[Exp]])
      }
    }

    // Evaluate as usual, but trap 0*0 as a special case
    def peval[T](t: Exp[(T, Int)])(implicit T: Recursive.Aux[T, Exp]): Int =
      t match {
        case Mul((Embed(Num(0)), _), (Embed(Num(0)), _)) => -1
        case Mul((_,             x), (_,             y)) => x * y
        case Num(x)                                      => x
        case _                                           => ???
      }

    "attributePara" >> {
      "provide a catamorphism" in {
        val v = mul(num(4), mul(num(2), num(3)))
        v.cata(attributePara(peval[Fix[Exp]])) should
          === (
            Cofree[Exp, Int](24, Now(Mul(
              Cofree(4, Now(Num(4))),
              Cofree(6, Now(Mul(
                Cofree(2, Now(Num(2))),
                Cofree(3, Now(Num(3))))))))))
      }
    }

    val weightedEval: ElgotAlgebraM[(Int, ?), Option, Exp, Int] = {
      case (weight, Num(x))    => (weight * x).some
      case (weight, Mul(x, y)) => (weight * x * y).some
      case (_,      _)         => None
    }

    // "attributeElgotM" >> {
    //   "fold to Cofree" in {
    //     type T = Cofree[Exp, Int]

    //     Cofree[Exp, Int](1, Now(Mul(
    //       Cofree(2, Now(Num(1))),
    //       Cofree(2, Now(Mul(
    //         Cofree(3, Now(Num(2))),
    //         Cofree(3, Now(Num(3)))))))))
    //       .transCataM(((_: EnvT[Int, Exp, T]).run) >>> attributeElgotM[(Int, ?), Option, T](weightedEval)) should
    //       === (
    //         Cofree[Exp, Int](216, Now(Mul(
    //           Cofree(2, Now(Num(1))),
    //           Cofree(108, Now(Mul(
    //             Cofree(6, Now(Num(2))),
    //             Cofree(9, Now(Num(3))))))))).some)
    //   }
    // }

    "para" >> {
      "evaluate simple expr" in {
        testRec(
          mul(num(1), mul(num(2), num(3))),
          new RecRunner[Exp, Int] {
            def run[T](implicit T: Recursive.Aux[T, Exp]) =
              _.para(peval[T]) should === (6)
          })
      }

      "evaluate special-case" in {
        testRec(
          mul(num(0), num(0)),
          new RecRunner[Exp, Int] {
            def run[T](implicit T: Recursive.Aux[T, Exp]) =
              _.para(peval[T]) should === (-1)
          })
      }

      "evaluate equiv" in {
        testRec(
          mul(num(0), mul(num(0), num(1))),
          new RecRunner[Exp, Int] {
            def run[T](implicit T: Recursive.Aux[T, Exp]) =
              _.para(peval[T]) should === (0)
          })
      }
    }

    "gpara" >> {
      "behave like para" in {
        mul(num(0), mul(num(0), num(1)))
          .gpara[Id, Int](distCata, exp => peval(exp.map(_.runEnvT))) should
            === (0)
      }
    }

    def depth[T[_[_]], F[_]]: (Int, F[T[F]]) => Int = (i, _) => i + 1

    def sequential[T[_[_]], F[_]]: (Int, F[T[F]]) => State[Int, Int] =
      (_, _) => State.get[Int] <* State.modify[Int](_ + 1)

    // "attributeTopDown" >> {
    //   "increase toward leaves" in {
    //     val v = mul(num(0), mul(num(0), num(1)))
    //     v.attributeTopDown(0)(depth) should === (
    //       Cofree[Exp, Int](1, Now(Mul(
    //         Cofree(2, Now(Num(0))),
    //         Cofree(2, Now(Mul(
    //           Cofree(3, Now(Num(0))),
    //           Cofree(3, Now(Num(1))))))))))
    //   }

    //   "increase toward leaves, ltr" in {
    //     val v = mul(num(0), mul(num(0), num(1)))
    //     v.attributeTopDownM[State[Int, ?], Cofree[Exp, Int], Int](0)(sequential).runA(0).value should
    //       === (
    //         Cofree[Exp, Int](0, Now(Mul(
    //           Cofree(1, Now(Num(0))),
    //           Cofree(2, Now(Mul(
    //             Cofree(3, Now(Num(0))),
    //             Cofree(4, Now(Num(1))))))))))
    //   }
    // }

    "distCata" >> {
      "behave like cata" in {
        val v = mul(num(0), mul(num(0), num(1)))
        v.gcata[Id, Int](distCata, eval) should === (v.cata(eval))
        v.convertTo[Mu[Exp]].gcata[Id, Int](distCata, eval) should === (v.cata(eval))
        v.convertTo[Nu[Exp]].gcata[Id, Int](distCata, eval) should === (v.cata(eval))
      }
    }

    "distPara" >> {
      "behave like para" in {
        val v = mul(num(0), mul(num(0), num(1)))
        v.gcata[(Fix[Exp], ?), Int](distPara, peval[Fix[Exp]]) should === (v.para(peval[Fix[Exp]]))
        v.convertTo[Mu[Exp]].gcata[(Mu[Exp], ?), Int](distPara, peval[Mu[Exp]]) should === (v.convertTo[Mu[Exp]].para(peval[Mu[Exp]]))
        v.convertTo[Nu[Exp]].gcata[(Nu[Exp], ?), Int](distPara, peval[Nu[Exp]]) should === (v.convertTo[Nu[Exp]].para(peval[Nu[Exp]]))
      }
    }

    def extract2s[T](implicit T: Corecursive.Aux[T, Exp])
        : Int => Exp[T \/ Int] = x =>
      if (x == 0) Num(x)
      else if (x % 2 == 0) Mul(-\/(Num[T](2).embed), \/-(x.toInt / 2))
      else Num(x)

    def extract2sAnd5[T](implicit T: Corecursive.Aux[T, Exp])
        : Int => T \/ Exp[Int] = x =>
      if (x <= 2) Num(x).asRight
      else if (x % 2 == 0) \/-(Mul(2, x / 2))
      else if (x % 5 == 0)
        Mul(Num[T](5).embed, Num[T](x / 5).embed).embed.asLeft
      else Num(x).asRight

    def extract2sNot5[T](x: Int)(implicit T: Corecursive.Aux[T, Exp]):
        Option[Exp[T \/ Int]] =
      if (x == 5) None else extract2s[T].apply(x).some

    def fact[T](x: Int)(implicit T: Corecursive.Aux[T, Exp]): Exp[T \/ Int] =
      if (x > 1) Mul(-\/(Num[T](x).embed), \/-(x - 1))
      else Num(x)


    "apomorphism" >> {
      "pull out factors of two" in {
        "apoM" in {
          "should be some" in {
            12.apoM[Fix[Exp]](extract2sNot5[Fix[Exp]]) should
              === (mul(num(2), mul(num(2), num(3))).some)
          }
          "should be none" in {
            10.apoM[Fix[Exp]](extract2sNot5[Fix[Exp]]) should === (None)
          }
        }
        "apo should be an optimization over apoM and be semantically equivalent" >> prop { i: Int =>
          if (i == 0) ok
          else
            i.apoM[Fix[Exp]].apply[Id, Exp](extract2s) should
              === (i.apo[Fix[Exp]](extract2s[Fix[Exp]]))
        }
      }
      "construct factorial" in {
        4.apo[Fix[Exp]](fact[Fix[Exp]]) should
          === (mul(num(4), mul(num(3), mul(num(2), num(1)))))
      }
    }

    "elgotApo" >> {
      "pull out factors of two and stop on 5" in {
        420.elgotApo[Fix[Exp]](extract2sAnd5[Fix[Exp]]) should
          === (mul(num(2), mul(num(2), mul(num(5), num(21)))))
      }
    }

    "anamorphism" >> {
      "pull out factors of two" in {
        "anaM" >> {
          def extractFactorsM(x: Int): Option[Exp[Int]] =
            if (x == 5) None else extractFactors(x).some
          "pull out factors of two" in {
            testCorec(
              12,
              new CorecRunner[Option, Exp, Int] {
                def run[T: Eq: Show](implicit T: Birecursive.Aux[T, Exp]) =
                  _.anaM[T](extractFactorsM) should
                    === (mul(num(2), mul(num(2), num(3))).convertTo[T].some)
              })
          }
          "fail if 5 is present" in {
            testCorec(
              10,
              new CorecRunner[Option, Exp, Int] {
                def run[T: Eq: Show](implicit T: Birecursive.Aux[T, Exp]) =
                  _.anaM[T](extractFactorsM) must beNone
              })
          }
        }
        "ana should be an optimization over anaM and be semantically equivalent" >> prop { i: Int =>
          testCorec(
            i,
            new CorecRunner[Id, Exp, Int] {
              def run[T: Eq: Show](implicit T: Birecursive.Aux[T, Exp]) =
                _.anaM[T][Id, Exp](extractFactors) should
                  === (i.ana[T](extractFactors))
            })
        }
      }
    }

    "distAna" >> {
      "behave like ana in gana" >> prop { (i: Int) =>
        testCorec(
          i,
          new CorecRunner[Id, Exp, Int] {
            def run[T: Eq: Show](implicit T: Birecursive.Aux[T, Exp]) =
              _.gana[T][Id, Exp](distAna, extractFactors) should
                === (i.ana[T](extractFactors))
          })
      }

      "behave like ana in elgotAna" >> prop { (i: Int) =>
        testCorec(
          i,
          new CorecRunner[Id, Exp, Int] {
            def run[T: Eq: Show](implicit T: Birecursive.Aux[T, Exp]) =
              _.elgotAna[T][Id, Exp](distAna, extractFactors) should
                === (i.ana[T](extractFactors))
          })
      }
    }

    "distApo" >> {
      "behave like apo in gana" >> prop { (i: Int) =>
        (i.gana[Fix[Exp]](distApo[Fix[Exp], Exp], extract2s[Fix[Exp]]) should
          === (i.apo[Fix[Exp]](extract2s[Fix[Exp]]))).toResult // and
        // (i.gana[Mu[Exp]](distApo[Mu[Exp], Exp], extract2s[Mu[Exp]]) should
        //   === (i.apo[Mu[Exp]](extract2s[Mu[Exp]]))).toResult and
        // (i.gana[Nu[Exp]](distApo[Nu[Exp], Exp], extract2s[Nu[Exp]]) should
        //   === (i.apo[Nu[Exp]](extract2s[Nu[Exp]]))).toResult
      }

      "behave like elgotApo in elgotAna" >> prop { (i: Int) =>
        (i.elgotAna[Fix[Exp]](distApo[Fix[Exp], Exp], extract2sAnd5[Fix[Exp]]) should
          === (i.elgotApo[Fix[Exp]](extract2sAnd5[Fix[Exp]]))).toResult // and
        // (i.elgotAna[Mu[Exp]](distApo[Mu[Exp], Exp], extract2sAnd5[Mu[Exp]]) should
        //   === (i.elgotApo[Mu[Exp]](extract2sAnd5[Mu[Exp]]))).toResult and
        // (i.elgotAna[Nu[Exp]](distApo[Nu[Exp], Exp], extract2sAnd5[Nu[Exp]]) should
        //   === (i.elgotApo[Nu[Exp]](extract2sAnd5[Nu[Exp]]))).toResult
      }
    }

    "hylo" >> {
      "factor and then evaluate" >> prop { (i: Int) =>
        i.hylo(eval, extractFactors) should === (i)
      }
    }

    "ghylo" >> {
      "behave like hylo with distCata/distAna" ! prop { (i: Int) =>
        i.ghylo[Id, Id](distCata, distAna, eval, extractFactors) should
          === (i.hylo(eval, extractFactors))
      }

      "behave like chrono with distHisto/distFutu" ! prop { (i: Int) =>
        i.ghylo[Cofree[Exp, ?], Free[Exp, ?]](
          distHisto, distFutu, partialEval[Fix[Exp]], extract2and3) should
          === (i.chrono(partialEval[Fix[Exp]], extract2and3))
      }
    }

    // "paraZygo" >> {
    //   "peval and strings" in {
    //     testRec(
    //       mul(mul(num(0), num(0)), mul(num(2), num(5))),
    //       new RecRunner[Exp, String] {
    //         def run[T](implicit T: Recursive.Aux[T, Exp]) =
    //           _.paraZygo(peval[T], strings) should
    //             === ("0 (0), 0 (0) (-1), 2 (2), 5 (5) (10)")
    //       })
    //   }
    // }

    sealed abstract class Nat[A]
    final case class Z[A]()        extends Nat[A]
    final case class S[A](prev: A) extends Nat[A]
    object Nat {
      implicit val NatTraverse: Traverse[Nat] = new Traverse[Nat] {
        def foldLeft[A, B](fa: Nat[A],b: B)(f: (B, A) => B): B = ???

        def foldRight[A, B](fa: Nat[A],lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) = ???

        def traverse[G[_], A, B](fa: Nat[A])(f: A => G[B])(implicit G: Applicative[G]) =
          fa match {
            case Z()  => G.pure(Z())
            case S(a) => f(a).map(S(_))
          }
      }
    }

    "mutu" >> {
      val toNat: Int => Fix[Nat] = _.ana[Fix[Nat]]({
        case 0 => Z(): Nat[Int]
        case n => S(n - 1): Nat[Int]
      })

      case class Even(even: Boolean)
      case class Odd(odd: Boolean)

      val isOdd: Nat[(Even, Odd)] => Odd = {
        case Z()             => Odd(false)
        case S((Even(b), _)) => Odd(b)
      }
      val isEven: Nat[(Odd, Even)] => Even = {
        case Z()            => Even(true)
        case S((Odd(b), _)) => Even(b)
      }

      "determine even" in {
        toNat(8).mutu(isOdd, isEven) must_== Even(true)
      }

      "determine odd" in {
        toNat(5).mutu(isEven, isOdd) must_== Odd(true)
      }

      "determine not even" in {
        toNat(7).mutu(isOdd, isEven) must_== Even(false)
      }
    }

    "histo" >> {
      "eval simple literal multiplication" in {
        mul(num(5), num(10)).histo(partialEval[Fix[Exp]]) should === (num(50))
        // mul(num(5), num(10)).histo(partialEval[Mu[Exp]]) should === (num(50).convertTo[Mu[Exp]])
        // mul(num(5), num(10)).histo(partialEval[Nu[Exp]]) should === (num(50).convertTo[Nu[Exp]])
      }

      "partially evaluate mul in lambda" in {
        lam('foo, mul(mul(num(4), num(7)), vari('foo))).histo(partialEval[Fix[Exp]]) should
          === (lam('foo, mul(num(28), vari('foo))))
        // lam('foo, mul(mul(num(4), num(7)), vari('foo))).histo(partialEval[Mu[Exp]]) should
        //   === (lam('foo, mul(num(28), vari('foo))).convertTo[Mu[Exp]])
        // lam('foo, mul(mul(num(4), num(7)), vari('foo))).histo(partialEval[Nu[Exp]]) should
        //   === (lam('foo, mul(num(28), vari('foo))).convertTo[Nu[Exp]])
      }
    }

    def extract2and3(x: Int): Exp[Free[Exp, Int]] =
      // factors all the way down
      if (x > 2 && x % 2 == 0) Mul(Free.pure(2), Free.pure(x/2))
      // factors once and then stops
      else if (x > 3 && x % 3 == 0)
        Mul(Free.liftF(Num(3)), Free.liftF(Num(x/3)))
      else Num(x)

    "postpro" >> {
      "extract original with identity ~>" in {
        (72.postpro[Fix[Exp]](FunctionK.id[Exp], extractFactors) should
          === (mul(num(2), mul(num(2), mul(num(2), num(9)))))).toResult // and
        // (72.postpro[Mu[Exp]](FunctionK.id[Exp], extractFactors) should
        //   === (mul(num(2), mul(num(2), mul(num(2), num(9)))).convertTo[Mu[Exp]])).toResult and
        // (72.postpro[Nu[Exp]](FunctionK.id[Exp], extractFactors) should
        //   === (mul(num(2), mul(num(2), mul(num(2), num(9)))).convertTo[Nu[Exp]])).toResult
      }

      "apply ~> repeatedly" in {
        (72.postpro[Fix[Exp]](MinusThree, extractFactors) should
          === (mul(num(-1), mul(num(-4), mul(num(-7), num(0)))))).toResult // and
        // (72.postpro[Mu[Exp]](MinusThree, extractFactors) should
        //   === (mul(num(-1), mul(num(-4), mul(num(-7), num(0)))).convertTo[Mu[Exp]])).toResult and
        // (72.postpro[Nu[Exp]](MinusThree, extractFactors) should
        //   === (mul(num(-1), mul(num(-4), mul(num(-7), num(0)))).convertTo[Nu[Exp]])).toResult
      }
    }

    "gpostpro" >> {
      "extract original with identity ~>" in {
        72.gpostpro[Fix[Exp]](distFutu[Exp], FunctionK.id, extract2and3) should
          === (mul(num(2), mul(num(2), mul(num(2), mul(num(3), num(3))))))
      }

      "apply ~> repeatedly" in {
        72.gpostpro[Fix[Exp]](distFutu[Exp], MinusThree, extract2and3) should
          === (mul(num(-1), mul(num(-4), mul(num(-7), mul(num(-9), num(-9))))))
      }
    }

    "futu" >> {
      "factor multiples of two" in {
        testCorec(
          8,
          new CorecRunner[Id, Exp, Int] {
            def run[T: Eq: Show](implicit T: Birecursive.Aux[T, Exp]) =
              _.futu[T](extract2and3) should === (mul(num(2), mul(num(2), num(2))).convertTo[T])
          })
      }

      "factor multiples of three" in {
        testCorec(
          81,
          new CorecRunner[Id, Exp, Int] {
            def run[T: Eq: Show](implicit T: Birecursive.Aux[T, Exp]) =
              _.futu[T](extract2and3) should
                === (mul(num(3), num(27)).convertTo[T])
          })
      }

      "factor 3 within 2" in {
        testCorec(
          324,
          new CorecRunner[Id, Exp, Int] {
            def run[T: Eq: Show](implicit T: Birecursive.Aux[T, Exp]) =
              _.futu[T](extract2and3) should
                === (mul(num(2), mul(num(2), mul(num(3), num(27)))).convertTo[T])
          })
      }
    }

    "chrono" >> {
      "factor and partially eval" >> prop { (i: Int) =>
        i.chrono(partialEval[Fix[Exp]], extract2and3) should === (num(i))
        // i.chrono(partialEval[Mu[Exp]], extract2and3) should === (num(i).convertTo[Mu[Exp]])
        // i.chrono(partialEval[Nu[Exp]], extract2and3) should === (num(i).convertTo[Nu[Exp]])
      }
    }
  }

  "Attr" >> {
    type T[A] = Cofree[Exp, A]

    // "attrSelf" >> {
    //   "annotate all" >> Prop.forAll(expGen) { exp =>
    //     exp.transCata[T[Mu[Exp]]](attrSelf[T[Mu[Exp]]].apply).elgotPara(universe) should ===
    //     (exp.elgotPara(universe).map(_.transCata[T[Mu[Exp]]](attrSelf[T[Mu[Exp]]].apply)))
    //   }
    // }

    // "convert" >> {
    //   "forget unit" >> Prop.forAll(expGen) { exp =>
    //     exp.transCata[T[Unit]](attrK[T[Unit]](())).cata(deattribute[Exp, Unit, Mu[Exp]](_.embed)) should === (exp)
    //   }
    // }

    "foldMap" >> {
      "zeros" >> Prop.forAll(expGen) { exp =>
        Foldable[Cofree[Exp, ?]].foldMap(exp.transCata[T[Int]](attrK[T[Int]](0)))(_ :: Nil) should
          === (exp.elgotPara(universe).map(Function.const(0)).toList)
      }

      // "selves" >> Prop.forAll(expGen) { exp =>
      //   Foldable[Cofree[Exp, ?]].foldMap(exp.transCata[T[Mu[Exp]]](attrSelf[T[Mu[Exp]]].apply))(_ :: Nil) should
      //     === (exp.elgotPara(universe).toList)
      // }
    }
  }

  "count" should {
    "return the number of instances in the structure" in {
      val exp = mul(mul(num(12), mul(num(12), num(8))), mul(num(12), num(8)))
      exp.elgotPara(count(num(12))) should === (3)
    }
  }

  "size" should {
    "return the number of nodes in the structure" in {
      val exp = mul(mul(num(12), mul(num(12), num(8))), mul(num(12), num(8)))
      exp.cata(turtles.size[Nat].apply).toInt should === (9)
    }
  }

  "height" should {
    "return the longest path from root to leaf" in {
      val exp = mul(mul(num(12), mul(num(12), num(8))), mul(num(12), num(8)))
      exp.cata(height) should === (3)
    }
  }

  "quicksort" should {
    "sort an arbitrary list" in {
      quicksort(List(8, 29, 2002394, 9, 902098, 329123092, 202, 0, 2, 198)) should
        === (List(0, 2, 8, 9, 29, 198, 202, 902098, 2002394, 329123092))
    }
  }

  "find" should {
    val exp = mul(mul(num(10), mul(num(11), num(7))), mul(num(12), num(8)))

    "return root-most instance that passes" in {
      exp.transAnaTM(turtles.find[Fix[Exp]] {
        case Embed(Mul(Embed(Num(_)), _)) => true
        case _                            => false
      }) should === (mul(num(10), mul(num(11), num(7))).asLeft)
    }

    "return leaf-most instance that passes" in {
      exp.transCataTM(turtles.find[Fix[Exp]] {
        case Embed(Mul(Embed(Num(_)), _)) => true
        case _                            => false
      }) should === (mul(num(11), num(7)).asLeft)
    }
  }

  "substitute" should {
    "replace equivalent forms" in {
      val exp = mul(mul(num(12), mul(num(12), num(8))), mul(num(12), num(8)))
      val res = mul(mul(num(12), num(92)), num(92))
      exp.transApoT(substitute(mul(num(12), num(8)), num(92))) should === (res)
    }

    "replace equivalent forms without re-replacing created forms" in {
      val exp = mul(mul(num(12), mul(num(12), num(8))), mul(num(12), num(8)))
      val res = mul(mul(num(12), num(8)), num(8))
      exp.transApoT(substitute(mul(num(12), num(8)), num(8))) should === (res)
    }

    "replace equivalent forms without re-replacing inserted forms" in {
      val exp = mul(mul(num(12), num(8)), num(8))
      val res = mul(mul(num(12), mul(num(12), num(8))), mul(num(12), num(8)))
      exp.transApoT(substitute(num(8), mul(num(12), num(8)))) should === (res)
    }
  }

  "recover" should {
    "handle “partially-folded” values" in {
      val exp =
        CoEnv[Int, Exp, Fix[CoEnv[Int, Exp, ?]]](\/-(Mul(
          CoEnv[Int, Exp, Fix[CoEnv[Int, Exp, ?]]](\/-(Mul(
            CoEnv(2.asLeft[Exp[Fix[CoEnv[Int, Exp, ?]]]]).embed,
            CoEnv[Int, Exp, Fix[CoEnv[Int, Exp, ?]]](\/-(Mul(
              CoEnv[Int, Exp, Fix[CoEnv[Int, Exp, ?]]](Num(3).asRight[Int]).embed,
              CoEnv(4.asLeft[Exp[Fix[CoEnv[Int, Exp, ?]]]]).embed))).embed))).embed,
          CoEnv[Int, Exp, Fix[CoEnv[Int, Exp, ?]]](\/-(Mul(
            CoEnv[Int, Exp, Fix[CoEnv[Int, Exp, ?]]](Num(5).asRight[Int]).embed,
            CoEnv(6.asLeft[Exp[Fix[CoEnv[Int, Exp, ?]]]]).embed))).embed))).embed
      exp.cata(recover(eval)) should === (720)
    }
  }

  def expGen = Gen.resize(100, corecursiveArbitrary[Mu[Exp], Exp].arbitrary)
}
