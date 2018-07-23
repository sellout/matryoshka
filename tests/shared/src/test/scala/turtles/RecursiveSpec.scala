/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

import slamdata.Predef.{Eq => _, _}
import turtles.data._
import turtles.exp._
import turtles.exp2._
import turtles.helpers._
import turtles.implicits._
import turtles.instances.fixedpoint.{Partial, partialMonad, PartialOps}
import turtles.patterns._
import turtles.runners._

import cats.{Apply => _, _}
import cats.arrow._
import cats.data._
import cats.free._

import scala.util.{ Either => \/, Left => -\/, Right => \/- }

class RecursiveSpec extends TurtlesSuite {

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

  def simplifyƒ[T](implicit T: Steppable.Aux[T, Exp])
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

  def addOneOrSimplifyƒ[T](implicit T: Steppable.Aux[T, Exp])
      : Exp[T] => Partial[Exp[T]] = {
    case t @ Num(_)    => addOneƒ(t).pure[Partial]
    case t @ Mul(_, _) => repeatedly[Partial[Exp[T]]](simplifyƒ[T]).apply(t)
    case t             => t.pure[Partial]
  }

  def extractLambdaƒ[T[_[_]]: SteppableT]
      : Exp[(T[Exp], T[Exp2])] => Exp2[T[Exp2]] = {
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
    (implicit T: Steppable.Aux[T, Exp])
      : T =
    t match {
      case Mul(x, y) => (x.head.project, y.head.project) match {
        case (Num(a), Num(b)) => Num[T](a * b).embed
        case _                => t.map(_.head).embed
      }
      case _ => t.map(_.head).embed
    }

  test("isLeaf should be true for simple literal") {
    num(1).isLeaf should === (true)
    num(1).convertTo[Mu[Exp]].isLeaf should === (true)
    num(1).convertTo[Nu[Exp]].isLeaf should === (true)
  }

  test("isLeaf should be false for expression") {
    mul(num(1), num(2)).isLeaf should === (false)
    mul(num(1), num(2)).convertTo[Mu[Exp]].isLeaf should === (false)
    mul(num(1), num(2)).convertTo[Nu[Exp]].isLeaf should === (false)
  }

  // test("children should be empty for simple literal") {
  //   num(1).children[List[Fix[Exp]]] should be empty;
  //   num(1).convertTo[Mu[Exp]].children[List[Mu[Exp]]] should be empty;
  //   num(1).convertTo[Nu[Exp]].children[List[Nu[Exp]]] should be empty
  // }

  test("children should contain sub-expressions") {
    mul(num(1), num(2)).children[List[Fix[Exp]]] should === (List(num(1), num(2)))
    mul(num(1), num(2)).convertTo[Mu[Exp]].children[List[Mu[Exp]]] should
    === (List(num(1), num(2)).map(_.convertTo[Mu[Exp]]))
    mul(num(1), num(2)).convertTo[Nu[Exp]].children[List[Nu[Exp]]] should
    === (List(num(1), num(2)).map(_.convertTo[Nu[Exp]]))
  }

  test("universe should be one for simple literal") {
    num(1).elgotPara(universe) should === (NonEmptyList.of(num(1)))
    num(1).convertTo[Mu[Exp]].elgotPara(universe) should
    === (NonEmptyList.of(num(1)).map(_.convertTo[Mu[Exp]]))
    num(1).convertTo[Nu[Exp]].elgotPara(universe) should
    === (NonEmptyList.of(num(1)).map(_.convertTo[Nu[Exp]]))
  }

  test("universe should contain root and sub-expressions") {
    mul(num(1), num(2)).elgotPara(universe) should
    === (NonEmptyList.of(mul(num(1), num(2)), num(1), num(2)))
    mul(num(1), num(2)).convertTo[Mu[Exp]].elgotPara(universe) should
    === (NonEmptyList.of(mul(num(1), num(2)), num(1), num(2)).map(_.convertTo[Mu[Exp]]))
    mul(num(1), num(2)).convertTo[Nu[Exp]].elgotPara(universe) should
    === (NonEmptyList.of(mul(num(1), num(2)), num(1), num(2)).map(_.convertTo[Nu[Exp]]))
  }

  test("transCata should change simple literal") {
    testRec(
      num(1),
      new RecRunner[Exp] {
        def run[T]
          (implicit TS: Steppable.Aux[T, Exp], TR: Recursive.Aux[T, Exp]) =
          _.transCata[Fix[Exp]](addOneƒ) should === (num(2))
      })
  }

  test("transCata should change sub-expressions") {
    testRec(
      mul(num(1), num(2)),
      new RecRunner[Exp] {
        def run[T]
          (implicit TS: Steppable.Aux[T, Exp], TR: Recursive.Aux[T, Exp]) =
          _.transCata[Fix[Exp]](addOneƒ) should === (mul(num(2), num(3)))
      })
  }

  test("transCata should be bottom-up") {
    (mul(num(0), num(1)).transCataM[Partial, Fix[Exp], Exp](addOneOrSimplifyƒ).unsafePerformSync should === (num(2)))
    (mul(num(1), num(2)).transCataM[Partial, Fix[Exp], Exp](addOneOrSimplifyƒ).unsafePerformSync should === (mul(num(2), num(3))))
  }

  test("transAna should change simple literal") {
    testCorec(
      num(1),
      new CorecRunner[Id, Exp, Fix[Exp]] {
        def run[T: Eq: Show]
          (implicit TS: Steppable.Aux[T, Exp], TC: Corecursive.Aux[T, Exp]) =
          _.transAna[T](addOneƒ) should === (num(2).convertTo[T])
      })
  }

  test("transAna should change sub-expressions") {
    testCorec(
      mul(num(1), num(2)),
      new CorecRunner[Id, Exp, Fix[Exp]] {
        def run[T: Eq: Show]
          (implicit TS: Steppable.Aux[T, Exp], TC: Corecursive.Aux[T, Exp]) =
          _.transAna[T](addOneƒ) should === (mul(num(2), num(3)).convertTo[T])
      })
  }

  test("transAna should be top-down") {
    mul(num(0), num(1)).transAnaM[Partial, Fix[Exp], Exp](addOneOrSimplifyƒ).unsafePerformSync should === (num(0))
    mul(num(1), num(2)).transAnaM[Partial, Fix[Exp], Exp](addOneOrSimplifyƒ).unsafePerformSync should === (num(2))
  }

  test("prepro should multiply original with identity ~>") {
    mul(num(1), mul(num(12), num(8)))
      .prepro(FunctionK.id[Exp], example1ƒ) should
    === (96.some)
  }

  test("prepro should apply ~> repeatedly") {
    mul(num(1), mul(num(12), num(8))).prepro(MinusThree, example1ƒ) should
    === (-24.some)
  }

  test("gprepro should multiply original with identity ~>") {
    lam('meh, mul(vari('meh), mul(num(10), num(8))))
      .gprepro[Cofree[Exp, ?], Fix[Exp]](
        distHisto, FunctionK.id[Exp], partialEval[Fix[Exp]]) should
    === (lam('meh, mul(vari('meh), num(80))))
  }

  test("gprepro should apply ~> repeatedly") {
    lam('meh, mul(vari('meh), mul(num(13), num(8))))
      .gprepro[Cofree[Exp, ?], Fix[Exp]](
        distHisto, MinusThree, partialEval[Fix[Exp]]) should
    === (lam('meh, mul(vari('meh), num(-4))))
  }

  test("transPrepro should change literal with identity ~>") {
    testRec(
      num(1),
      new RecRunner[Exp] {
        def run[T]
          (implicit TS: Steppable.Aux[T, Exp], TR: Recursive.Aux[T, Exp]) =
          _.transPrepro[Fix[Exp]](FunctionK.id, addOneƒ) should
        === (num(2))
      })
  }

  test("transPrepro should apply ~> in original space") {
    testRec(
      mul(num(1), mul(num(12), num(8))),
      new RecRunner[Exp] {
        def run[T]
          (implicit TS: Steppable.Aux[T, Exp], TR: Recursive.Aux[T, Exp]) =
          _.transPrepro[Fix[Exp]](MinusThree, addOneƒ) should
        === (mul(num(-1), mul(num(7), num(3))))
      })
  }

  test("transPrepro should apply ~> with change of space") {
    testRec(
      num(1),
      new RecRunner[Exp] {
        def run[T]
          (implicit TS: Steppable.Aux[T, Exp], TR: Recursive.Aux[T, Exp]) =
          _.transPrepro[Fix[Exp2]](MinusThree, addOneExpExp2ƒ) should === (num2(2))
      })
  }

  test("transPostpro should change literal with identity ~>") {
    testCorec(
      num(1),
      new CorecRunner[Id, Exp, Fix[Exp]] {
        def run[T: Eq: Show]
          (implicit TS: Steppable.Aux[T, Exp], TC: Corecursive.Aux[T, Exp]) =
          _.transPostpro[T](FunctionK.id, addOneƒ) should
        === (num(2).convertTo[T])
      })
  }

  test("transPostpro should apply ~> in original space") {
    testCorec(
      mul(num(1), mul(num(12), num(8))),
      new CorecRunner[Id, Exp, Fix[Exp]] {
        def run[T: Eq: Show]
          (implicit TS: Steppable.Aux[T, Exp], TC: Corecursive.Aux[T, Exp]) =
          _.transPostpro[T](MinusThree, addOneƒ) should
        === (mul(num(-1), mul(num(7), num(3))).convertTo[T])
      })
  }

  test("transPostpro should apply ~> with change of space") {
    testCorec(
      num2(1),
      new CorecRunner[Id, Exp, Fix[Exp2]] {
        def run[T: Eq: Show]
          (implicit TS: Steppable.Aux[T, Exp], TC: Corecursive.Aux[T, Exp]) =
          _.transPostpro[T](MinusThree, addOneExp2Expƒ) should
        === (num(2).convertTo[T])
      })
  }

  test("transPara should project basic exp") {
    lam('sym, num(3)).transPara[Fix[Exp2]](extractLambdaƒ[Fix]) should === (num2(3))
  }

  test("transPara should project basic exp recursively") {
    lam('sym, mul(num(5), num(7))).transPara[Fix[Exp2]](extractLambdaƒ[Fix]) should
    === (single(const))
  }

  test("foldMap should fold stuff") {
    mul(num(0), num(1)).foldMap(_ :: Nil) should === (mul(num(0), num(1)) :: num(0) :: num(1) :: Nil)
  }

  val findConstants: Exp[List[Int]] => List[Int] = {
    case Num(x) => x :: Nil
    case t      => t.fold
  }

  test("cata should evaluate simple expr") {
    testRec(
      mul(num(1), mul(num(2), num(3))),
      new RecRunner[Exp] {
        def run[T]
          (implicit TS: Steppable.Aux[T, Exp], TR: Recursive.Aux[T, Exp]) =
          _.cata(eval) should === (6)
      })
  }

  test("cata should find all constants") {
    testRec(
      mul(num(0), num(1)),
      new RecRunner[Exp] {
        def run[T]
          (implicit TS: Steppable.Aux[T, Exp], TR: Recursive.Aux[T, Exp]) =
          _.cata(findConstants) should === (List(0, 1))
      })
  }

  test("cata should produce correct annotations for 5 * 2") {
    testRec(
      mul(num(5), num(2)),
      new RecRunner[Exp] {
        def run[T]
          (implicit TS: Steppable.Aux[T, Exp], TR: Recursive.Aux[T, Exp]) =
          _.cata(example1ƒ) should === (Some(10))
      })
  }

  test("zipAlgebras should both eval and find all constants") {
    testRec(
      mul(num(5), num(2)),
      new RecRunner[Exp] {
        def run[T]
          (implicit TS: Steppable.Aux[T, Exp], TR: Recursive.Aux[T, Exp]) =
          _.cata(AlgebraSemigroupal[Exp].product(eval, findConstants)) should
        === ((10, List(5, 2)))
      })
  }

  test("generalize should behave like cata") {
    testRec(
      mul(num(1), mul(num(2), num(3))),
      new RecRunner[Exp] {
        def run[T]
          (implicit TS: Steppable.Aux[T, Exp], TR: Recursive.Aux[T, Exp]) = t =>
        t.para(eval.generalize[(T, ?)]) should === (t.cata(eval))
      })
  }

  test("coelgot should behave like cofCata <<< attributeAna") {
    forAll { (i: Int) =>
      i.coelgot(eval.generalizeElgot[(Int, ?)], extractFactors) should === (
        i.ana[Cofree[Exp, Int]](attributeCoalgebra(extractFactors)).cata(liftT(eval.generalizeElgot[(Int, ?)])))
    }
  }

  test("elgot should behave like interpCata <<< freeAna") {
    forAll { (i: Int) =>
      i.elgot(eval, extractFactors.generalizeElgot[Int \/ ?]) should === (
        i.ana[Free[Exp, Int]](runT(extractFactors.generalizeElgot[Int \/ ?])).cata(patterns.recover(eval)))
    }
  }

  test("generalizeElgot should behave like cata on an algebra") { forAll { (i: Int) =>
    val x = i.ana[Fix[Exp]](extractFactors).cata(eval)
    i.coelgot(eval.generalizeElgot[(Int, ?)], extractFactors) should === (x)
  }
  }

  test("generalizeElgot should behave like ana on an coalgebra") {
    forAll { (i: Int) =>
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

  test("elgotCata should find no free vars in closed term") {
    val t = lam('sym, mul(num(5), vari('sym)))
    t.elgotCata(distZygo(freeVars), freeVarsNumber) should === (0)
  }

  test("elgotCata should find some free vars in open term") {
    val t1 = lam('bound, mul(vari('open), vari('bound)))
    val t2 = lam('x, mul(vari('y), lam('y, mul(vari('open), mul(vari('y), vari('x))))))
    t1.elgotCata(distZygo(freeVars), freeVarsNumber) should === (1)
    t2.elgotCata(distZygo(freeVars), freeVarsNumber) should === (2)
  }

  test("elgotAna should generate closed terms") {
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
  test("elgotAna should generate open terms") {
    (3, 1).elgotAna[Fix[Exp]].apply(distGApo(generateVars), generateTerm) should ===  (
      mul(mul(vari('x0), vari('x0)), vari('x0))
    )
  }

  val freeVarsDistr = distZygo[Exp, List[Symbol]](freeVars)
  val varGenDistr = distGApo[Exp, Int](generateVars)
  def closed(n: Int, bound: Int) =
    elgotHylo((n, bound))(freeVarsDistr, varGenDistr, freeVarsNumber, generateTerm)

  test("elgotHylo should assert that closed terms don't have free vars") {
    for (i <- (0 to 10)) {
      closed(i, 0) should ===  (0)
    }
    closed(11, 0) should ===  (0)
  }

  test("generalizeCoalgebra should behave like ana") { forAll { (i: Int) =>
    i.apo[Fix[Exp]](extractFactors.generalize[Fix[Exp] \/ ?]) should
    === (i.ana[Fix[Exp]](extractFactors))
    i.apo[Mu[Exp]](extractFactors.generalize[Mu[Exp] \/ ?]) should
    === (i.ana[Mu[Exp]](extractFactors))
    i.apo[Nu[Exp]](extractFactors.generalize[Nu[Exp] \/ ?]) should
    === (i.ana[Nu[Exp]](extractFactors))
  }
  }

  def subst[T]
    (vars: Map[Symbol, T], t: T)
    (implicit T: Steppable.Aux[T, Exp])
      : (Map[Symbol, T], T) = t.project match {
    case Let(sym, value, body) => (vars + ((sym, value)), body)
    case Var(sym)              => (vars, vars.get(sym).getOrElse(t))
    case _                     => (vars, t)
  }

  test("topDownCata should bind vars") {
    val v = let('x, num(1), mul(num(0), vari('x)))
    v.topDownCata(Map.empty[Symbol, Fix[Exp]])(subst) should
    === (mul(num(0), num(1)))
    v.convertTo[Mu[Exp]].topDownCata(Map.empty[Symbol, Mu[Exp]])(subst) should
    === (mul(num(0), num(1)).convertTo[Mu[Exp]])
    v.convertTo[Nu[Exp]].topDownCata(Map.empty[Symbol, Nu[Exp]])(subst) should
    === (mul(num(0), num(1)).convertTo[Nu[Exp]])
  }

  // Evaluate as usual, but trap 0*0 as a special case
  def peval[T](t: Exp[(T, Int)])(implicit T: Steppable.Aux[T, Exp]): Int =
    t match {
      case Mul((Embed(Num(0)), _), (Embed(Num(0)), _)) => -1
      case Mul((_,             x), (_,             y)) => x * y
      case Num(x)                                      => x
      case _                                           => ???
    }

  test("attributePara should provide a catamorphism") {
    val v = mul(num(4), mul(num(2), num(3)))
    v.cata(attributePara(peval[Fix[Exp]])) should
    === (
      Cofree[Exp, Int](24, Now(Mul(
        Cofree(4, Now(Num(4))),
        Cofree(6, Now(Mul(
          Cofree(2, Now(Num(2))),
          Cofree(3, Now(Num(3))))))))))
  }

  val weightedEval: ElgotAlgebraM[(Int, ?), Option, Exp, Int] = {
    case (weight, Num(x))    => (weight * x).some
    case (weight, Mul(x, y)) => (weight * x * y).some
    case (_,      _)         => None
  }

  test("attributeElgotM should fold to Cofree") {
    type T = Cofree[Exp, Int]

    Cofree[Exp, Int](1, Now(Mul(
      Cofree(2, Now(Num(1))),
      Cofree(2, Now(Mul(
        Cofree(3, Now(Num(2))),
        Cofree(3, Now(Num(3)))))))))
      .transCataM(((_: EnvT[Int, Exp, T]).run) >>> attributeElgotM[(Int, ?), Option, T](weightedEval)) should
    === (
      Cofree[Exp, Int](216, Now(Mul(
        Cofree(2, Now(Num(1))),
        Cofree(108, Now(Mul(
          Cofree(6, Now(Num(2))),
          Cofree(9, Now(Num(3))))))))).some)
  }

  test("para should evaluate simple expr") {
    testRec(
      mul(num(1), mul(num(2), num(3))),
      new RecRunner[Exp] {
        def run[T]
          (implicit TS: Steppable.Aux[T, Exp], TR: Recursive.Aux[T, Exp]) =
          _.para(peval[T]) should === (6)
      })
  }

  test("para should evaluate special-case") {
    testRec(
      mul(num(0), num(0)),
      new RecRunner[Exp] {
        def run[T]
          (implicit TS: Steppable.Aux[T, Exp], TR: Recursive.Aux[T, Exp]) =
          _.para(peval[T]) should === (-1)
      })
  }

  test("para should evaluate equiv") {
    testRec(
      mul(num(0), mul(num(0), num(1))),
      new RecRunner[Exp] {
        def run[T](implicit TS: Steppable.Aux[T, Exp], TR: Recursive.Aux[T, Exp]) =
          _.para(peval[T]) should === (0)
      })
  }

  test("paraT should behave like para") {
    mul(num(0), mul(num(0), num(1)))
      .paraT[Id, Int](distCata, exp => peval(exp.map(_.runEnvT))) should
    === (0)
  }

  def depth[T[_[_]], F[_]]: (Int, F[T[F]]) => Int = (i, _) => i + 1

  def sequential[T[_[_]], F[_]]: (Int, F[T[F]]) => State[Int, Int] =
    (_, _) => State.get[Int] <* State.modify[Int](_ + 1)

  test("attributeTopDown should increase toward leaves") {
    val v = mul(num(0), mul(num(0), num(1)))
    v.attributeTopDown(0)(depth) should === (
      Cofree[Exp, Int](1, Now(Mul(
        Cofree(2, Now(Num(0))),
        Cofree(2, Now(Mul(
          Cofree(3, Now(Num(0))),
          Cofree(3, Now(Num(1))))))))))
  }

  test("attributeTopDownM should increase toward leaves, ltr") {
    val v = mul(num(0), mul(num(0), num(1)))
    v.attributeTopDownM[State[Int, ?], Cofree[Exp, Int], Int](0)(sequential).runA(0).value should
    === (
      Cofree[Exp, Int](0, Now(Mul(
        Cofree(1, Now(Num(0))),
        Cofree(2, Now(Mul(
          Cofree(3, Now(Num(0))),
          Cofree(4, Now(Num(1))))))))))
  }

  test("distCata should behave like cata") {
    val v = mul(num(0), mul(num(0), num(1)))
    v.gcata[Id, Int](distCata, eval) should === (v.cata(eval))
    v.convertTo[Mu[Exp]].gcata[Id, Int](distCata, eval) should === (v.cata(eval))
    v.convertTo[Nu[Exp]].gcata[Id, Int](distCata, eval) should === (v.cata(eval))
  }

  def extract2s[T](implicit T: Steppable.Aux[T, Exp])
      : Int => Exp[T \/ Int] = x =>
  if (x == 0) Num(x)
  else if (x % 2 == 0) Mul(-\/(Num[T](2).embed), \/-(x.toInt / 2))
  else Num(x)

  def extract2sAnd5[T](implicit T: Steppable.Aux[T, Exp])
      : Int => T \/ Exp[Int] = x =>
  if (x <= 2) Num(x).asRight
  else if (x % 2 == 0) \/-(Mul(2, x / 2))
  else if (x % 5 == 0)
    Mul(Num[T](5).embed, Num[T](x / 5).embed).embed.asLeft
  else Num(x).asRight

  def extract2sNot5[T](x: Int)(implicit T: Steppable.Aux[T, Exp]):
      Option[Exp[T \/ Int]] =
    if (x == 5) None else extract2s[T].apply(x).some

  def fact[T](x: Int)(implicit T: Steppable.Aux[T, Exp]): Exp[T \/ Int] =
    if (x > 1) Mul(-\/(Num[T](x).embed), \/-(x - 1))
    else Num(x)


  test("apoM should pull out some factors of two") {
    12.apoM[Fix[Exp]](extract2sNot5[Fix[Exp]]) should
    === (mul(num(2), mul(num(2), num(3))).some)
  }
  test("apoM should pull out no factors") {
    10.apoM[Fix[Exp]](extract2sNot5[Fix[Exp]]) should === (None)
  }
  // test("apo should be an optimization over apoM and be semantically equivalent") {
  //   forAll { i: Int =>
  //     if (i == 0) ok
  //     else
  //       i.apoM[Fix[Exp]].apply[Id, Exp](extract2s) should
  //     === (i.apo[Fix[Exp]](extract2s[Fix[Exp]]))
  //   }
  // }
  test("apo should construct factorial") {
    4.apo[Fix[Exp]](fact[Fix[Exp]]) should
    === (mul(num(4), mul(num(3), mul(num(2), num(1)))))
  }

  test("elgotApo should pull out factors of two and stop on 5") {
    420.elgotApo[Fix[Exp]](extract2sAnd5[Fix[Exp]]) should
      === (mul(num(2), mul(num(2), mul(num(5), num(21)))))
  }

  def extractFactorsM(x: Int): Option[Exp[Int]] =
    if (x == 5) None else extractFactors(x).some
  test("anaM should pull out factors of two") {
    testCorec(
      12,
      new CorecRunner[Option, Exp, Int] {
        def run[T: Eq: Show]
          (implicit TS: Steppable.Aux[T, Exp], TC: Corecursive.Aux[T, Exp]) =
          _.anaM[T](extractFactorsM) should
        === (mul(num(2), mul(num(2), num(3))).convertTo[T].some)
      })
  }
  test("anaM should fail if 5 is present") {
    testCorec(
      10,
      new CorecRunner[Option, Exp, Int] {
        def run[T: Eq: Show]
          (implicit TS: Steppable.Aux[T, Exp], TC: Corecursive.Aux[T, Exp]) =
          _.anaM[T](extractFactorsM) should === (None)
      })
  }
  test("ana should be an optimization over anaM and be semantically equivalent") {
    forAll { i: Int =>
      testCorec(
        i,
        new CorecRunner[Id, Exp, Int] {
          def run[T: Eq: Show]
            (implicit TS: Steppable.Aux[T, Exp], TC: Corecursive.Aux[T, Exp]) =
            _.anaM[T][Id, Exp](extractFactors) should
          === (i.ana[T](extractFactors))
        })
    }
  }

  test("distAna should behave like ana in gana") {
    forAll { (i: Int) =>
      testCorec(
        i,
        new CorecRunner[Id, Exp, Int] {
          def run[T: Eq: Show]
            (implicit TS: Steppable.Aux[T, Exp], TC: Corecursive.Aux[T, Exp]) =
            _.gana[T][Id, Exp](distAna, extractFactors) should
              === (i.ana[T](extractFactors))
        })
    }
  }

  test("distAna should behave like ana in elgotAna") {
    forAll { (i: Int) =>
      testCorec(
        i,
        new CorecRunner[Id, Exp, Int] {
          def run[T: Eq: Show]
            (implicit TS: Steppable.Aux[T, Exp], TC: Corecursive.Aux[T, Exp]) =
            _.elgotAna[T][Id, Exp](distAna, extractFactors) should
              === (i.ana[T](extractFactors))
        })
    }
  }

  test("hylo should factor and then evaluate") {
    forAll { (i: Int) =>
      i.hylo(eval, extractFactors) should === (i)
    }
  }

  test("ghylo should behave like hylo with distCata/distAna") {
    forAll { (i: Int) =>
      i.ghylo[Id, Id](distCata, distAna, eval, extractFactors) should
        === (i.hylo(eval, extractFactors))
    }
  }

  test("paraZygo should peval and strings") {
    testRec(
      mul(mul(num(0), num(0)), mul(num(2), num(5))),
      new RecRunner[Exp] {
        def run[T]
          (implicit TS: Steppable.Aux[T, Exp], TR: Recursive.Aux[T, Exp]) =
          _.paraZygo(peval[T], strings) should
            === ("0 (0), 0 (0) (-1), 2 (2), 5 (5) (10)")
      })
  }

  sealed abstract class Nat[A]
  case class Z[A]()        extends Nat[A]
  case class S[A](prev: A) extends Nat[A]
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

  // test("mutu should determine even") {
  //   toNat(8).mutu(isOdd, isEven) should === (Even(true))
  // }

  // test("mutu should determine odd") {
  //   toNat(5).mutu(isEven, isOdd) should === (Odd(true))
  // }

  // test("mutu should determine not even") {
  //   toNat(7).mutu(isOdd, isEven) should === (Even(false))
  // }

  test("histo should eval simple literal multiplication") {
    mul(num(5), num(10)).histo(partialEval[Fix[Exp]]) should === (num(50))
    mul(num(5), num(10)).histo(partialEval[Mu[Exp]]) should === (num(50).convertTo[Mu[Exp]])
    mul(num(5), num(10)).histo(partialEval[Nu[Exp]]) should === (num(50).convertTo[Nu[Exp]])
  }

  test("histo should partially evaluate mul in lambda") {
    lam('foo, mul(mul(num(4), num(7)), vari('foo))).histo(partialEval[Fix[Exp]]) should
    === (lam('foo, mul(num(28), vari('foo))))
    lam('foo, mul(mul(num(4), num(7)), vari('foo))).histo(partialEval[Mu[Exp]]) should
    === (lam('foo, mul(num(28), vari('foo))).convertTo[Mu[Exp]])
    lam('foo, mul(mul(num(4), num(7)), vari('foo))).histo(partialEval[Nu[Exp]]) should
    === (lam('foo, mul(num(28), vari('foo))).convertTo[Nu[Exp]])
  }

  def extract2and3(x: Int): Exp[Free[Exp, Int]] =
    // factors all the way down
    if (x > 2 && x % 2 == 0) Mul(Free.pure(2), Free.pure(x/2))
    // factors once and then stops
    else if (x > 3 && x % 3 == 0)
      Mul(Free.liftF(Num(3)), Free.liftF(Num(x/3)))
    else Num(x)

  test("postpro should extract original with identity ~>") {
    (72.postpro[Fix[Exp]](FunctionK.id[Exp], extractFactors) should
      === (mul(num(2), mul(num(2), mul(num(2), num(9))))))
    (72.postpro[Mu[Exp]](FunctionK.id[Exp], extractFactors) should
      === (mul(num(2), mul(num(2), mul(num(2), num(9)))).convertTo[Mu[Exp]]))
    (72.postpro[Nu[Exp]](FunctionK.id[Exp], extractFactors) should
      === (mul(num(2), mul(num(2), mul(num(2), num(9)))).convertTo[Nu[Exp]]))
  }

  test("postpro should apply ~> repeatedly") {
    (72.postpro[Fix[Exp]](MinusThree, extractFactors) should
      === (mul(num(-1), mul(num(-4), mul(num(-7), num(0))))))
    (72.postpro[Mu[Exp]](MinusThree, extractFactors) should
      === (mul(num(-1), mul(num(-4), mul(num(-7), num(0)))).convertTo[Mu[Exp]]))
    (72.postpro[Nu[Exp]](MinusThree, extractFactors) should
      === (mul(num(-1), mul(num(-4), mul(num(-7), num(0)))).convertTo[Nu[Exp]]))
  }

  test("gpostpro should extract original with identity ~>") {
    72.gpostpro[Fix[Exp]](distFutu[Exp], FunctionK.id, extract2and3) should
    === (mul(num(2), mul(num(2), mul(num(2), mul(num(3), num(3))))))
  }

  test("gpostpro should apply ~> repeatedly") {
    72.gpostpro[Fix[Exp]](distFutu[Exp], MinusThree, extract2and3) should
    === (mul(num(-1), mul(num(-4), mul(num(-7), mul(num(-9), num(-9))))))
  }

  test("futu should factor multiples of two") {
    testCorec(
      8,
      new CorecRunner[Id, Exp, Int] {
        def run[T: Eq: Show]
          (implicit TS: Steppable.Aux[T, Exp], TC: Corecursive.Aux[T, Exp]) =
          _.futu[T](extract2and3) should === (mul(num(2), mul(num(2), num(2))).convertTo[T])
      })
  }

  test("futu should factor multiples of three") {
    testCorec(
      81,
      new CorecRunner[Id, Exp, Int] {
        def run[T: Eq: Show]
          (implicit TS: Steppable.Aux[T, Exp], TC: Corecursive.Aux[T, Exp]) =
          _.futu[T](extract2and3) should
            === (mul(num(3), num(27)).convertTo[T])
      })
  }

  test("futu should factor 3 within 2") {
    testCorec(
      324,
      new CorecRunner[Id, Exp, Int] {
        def run[T: Eq: Show]
          (implicit TS: Steppable.Aux[T, Exp], TC: Corecursive.Aux[T, Exp]) =
          _.futu[T](extract2and3) should
            === (mul(num(2), mul(num(2), mul(num(3), num(27)))).convertTo[T])
      })
  }

  test ("chrono should factor and partially eval") {
    forAll { (i: Int) =>
      i.chrono(partialEval[Fix[Exp]], extract2and3) should === (num(i))
      i.chrono(partialEval[Mu[Exp]], extract2and3) should === (num(i).convertTo[Mu[Exp]])
      i.chrono(partialEval[Nu[Exp]], extract2and3) should === (num(i).convertTo[Nu[Exp]])
    }
  }
}
