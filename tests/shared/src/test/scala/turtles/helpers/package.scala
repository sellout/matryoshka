/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

import slamdata.Predef.{Eq => _, _}
import turtles.exp._

import cats._
import cats.data._
import cats.implicits._
import org.scalacheck._

package object helpers {

  def foldableCogen[F[_]: Foldable]: Delay[Cogen, F] =
    new Delay[Cogen, F] {
      def apply[A](cog: Cogen[A]): Cogen[F[A]] =
        Cogen((seed, value) => value.foldLeft(seed)(cog.perturb))
    }

  implicit val nonEmptyListCogen: Delay[Cogen, NonEmptyList] = foldableCogen

  implicit def nonEmptyListEq: Delay[Eq, NonEmptyList] =
    new Delay[Eq, NonEmptyList] {
      def apply[A](eq: Eq[A]) = NonEmptyList.catsDataEqForNonEmptyList(eq)
    }

  def strings(t: Exp[(Int, String)]): String = t match {
    case Num(x) => x.toString
    case Mul((x, xs), (y, ys)) =>
      xs + " (" + x + ")" + ", " + ys + " (" + y + ")"
    case _ => ???
  }

  val eval: Algebra[Exp, Int] = {
    case Num(x)    => x
    case Mul(x, y) => x * y
    case _         => ???
  }

  // Evaluate as usual, but trap 0*0 as a special case
  def peval[T](t: Exp[(T, Int)])(implicit T: Steppable.Aux[T, Exp]): Int =
    t match {
      case Mul((Embed(Num(0)), _), (Embed(Num(0)), _)) => -1
      case Mul((_,             x), (_,             y)) => x * y
      case Num(x)                                      => x
      case _                                           => ???
    }

  def elgotStrings: ElgotAlgebra[(Int, ?), Exp, String] = {
    case (x, Num(xs)) if x == xs => x.toString
    case (x, Mul(xs, ys)) => "(" + xs + " * " + ys + " = " + x + ")"
    case _ => ???
  }

  def extractFactors: Coalgebra[Exp, Int] = x =>
    if (x > 2 && x % 2 == 0) Mul(2, x/2)
    else Num(x)
}
