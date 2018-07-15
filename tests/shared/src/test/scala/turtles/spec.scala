/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

import slamdata.Predef.{Eq => _, _}
import turtles.data._
import turtles.exp._
import turtles.helpers._
import turtles.implicits._
import turtles.instances.fixedpoint.{Nat, RecursiveOptionOps}
import turtles.patterns._
// import turtles.runners._

// import cats.{Apply => _, _}
// import cats.arrow._
// import cats.data._
// import cats.free._
// import cats.implicits._
// import cats.laws.discipline._
import org.scalacheck._

import scala.util.{ Right => \/- }

// class ExpSpec extends TurtlesSuite {
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

class TurtlesSpecs extends TurtlesSuite {


  checkAlgebraIsoLaws("birec", Birecursive.iso[Mu[Exp], Exp])
  checkAlgebraIsoLaws("lambek", Birecursive.lambekIso[Mu[Exp], Exp])

  // "Attr" >> {
  //   type T[A] = Cofree[Exp, A]

  //   "attrSelf" >> {
  //     "annotate all" >> Prop.forAll(expGen) { exp =>
  //       exp.transCata[T[Mu[Exp]]](attrSelf[T[Mu[Exp]]].apply).elgotPara(universe) should ===
  //       (exp.elgotPara(universe).map(_.transCata[T[Mu[Exp]]](attrSelf[T[Mu[Exp]]].apply)))
  //     }
  //   }

  //   "convert" >> {
  //     "forget unit" >> Prop.forAll(expGen) { exp =>
  //       exp.transCata[T[Unit]](attrK[T[Unit]](())).cata(deattribute[Exp, Unit, Mu[Exp]](_.embed)) should === (exp)
  //     }
  //   }

  //   "foldMap" >> {
  //     "zeros" >> Prop.forAll(expGen) { exp =>
  //       Foldable[Cofree[Exp, ?]].foldMap(exp.transCata[T[Int]](attrK[T[Int]](0)))(_ :: Nil) should
  //         === (exp.elgotPara(universe).map(Function.const(0)).toList)
  //     }

  //     "selves" >> Prop.forAll(expGen) { exp =>
  //       Foldable[Cofree[Exp, ?]].foldMap(exp.transCata[T[Mu[Exp]]](attrSelf[T[Mu[Exp]]].apply))(_ :: Nil) should
  //         === (exp.elgotPara(universe).toList)
  //     }
  //   }
  // }

  test("count should return the number of instances in the structure") {
    val exp = mul(mul(num(12), mul(num(12), num(8))), mul(num(12), num(8)))
    exp.elgotPara(count(num(12))) should === (3)
  }

  test("size should return the number of nodes in the structure") {
    val exp = mul(mul(num(12), mul(num(12), num(8))), mul(num(12), num(8)))
    exp.cata(turtles.size[Nat].apply).toInt should === (9)
  }

  test("height should return the longest path from root to leaf") {
    val exp = mul(mul(num(12), mul(num(12), num(8))), mul(num(12), num(8)))
    exp.cata(height) should === (3)
  }

  test("quicksort should sort an arbitrary list") {
    quicksort(List(8, 29, 2002394, 9, 902098, 329123092, 202, 0, 2, 198)) should
    === (List(0, 2, 8, 9, 29, 198, 202, 902098, 2002394, 329123092))
  }

  val exp = mul(mul(num(10), mul(num(11), num(7))), mul(num(12), num(8)))

  test("find should return root-most instance that passes") {
    exp.transAnaTM(turtles.find[Fix[Exp]] {
      case Embed(Mul(Embed(Num(_)), _)) => true
      case _                            => false
    }) should === (mul(num(10), mul(num(11), num(7))).asLeft)
  }

  test("find should return leaf-most instance that passes") {
    exp.transCataTM(turtles.find[Fix[Exp]] {
      case Embed(Mul(Embed(Num(_)), _)) => true
      case _                            => false
    }) should === (mul(num(11), num(7)).asLeft)
  }

  test("substitute should replace equivalent forms") {
    val exp = mul(mul(num(12), mul(num(12), num(8))), mul(num(12), num(8)))
    val res = mul(mul(num(12), num(92)), num(92))
    exp.transApoT(substitute(mul(num(12), num(8)), num(92))) should === (res)
  }

  test("substitute should replace equivalent forms without re-replacing created forms") {
    val exp = mul(mul(num(12), mul(num(12), num(8))), mul(num(12), num(8)))
    val res = mul(mul(num(12), num(8)), num(8))
    exp.transApoT(substitute(mul(num(12), num(8)), num(8))) should === (res)
  }

  test("substitute should replace equivalent forms without re-replacing inserted forms") {
    val exp = mul(mul(num(12), num(8)), num(8))
    val res = mul(mul(num(12), mul(num(12), num(8))), mul(num(12), num(8)))
    exp.transApoT(substitute(num(8), mul(num(12), num(8)))) should === (res)
  }

  test("recover should handle “partially-folded” values") {
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

  def expGen = Gen.resize(100, corecursiveArbitrary[Mu[Exp], Exp].arbitrary)
}
