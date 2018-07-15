/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package matryoshka.example

import slamdata.Predef._
import turtles._
import turtles.data._
import turtles.implicits._

import cats._
import org.specs2.mutable._

sealed trait Expr[A]

object Expr {

  case class Lit[A](value: Int) extends Expr[A]
  case class Add[A](x: A, y: A) extends Expr[A]
  case class Multiply[A](x: A, y: A) extends Expr[A]

  implicit val exprFunctor: Functor[Expr] = new Functor[Expr] {
    def map[A, B](expr: Expr[A])(f: A => B): Expr[B] = expr match {
      case Lit(v) => Lit(v)
      case Add(x, y) => Add(f(x), f(y))
      case Multiply(x, y) => Multiply(f(x), f(y))
    }
  }

  val evaluate: Algebra[Expr, Int] = {
    case Lit(v) => v
    case Add(x, y) => x + y
    case Multiply(x, y) => x * y
  }

  val show: Algebra[Expr, String] = {
    case Lit(v) => s"$v"
    case Add(x, y) => s"($x + $y)"
    case Multiply(x, y) => s"($x * $y)"
  }

  def lit(value: Int): Fix[Expr] = Fix(Lit(value))
  def multiply(x: Fix[Expr], y: Fix[Expr]): Fix[Expr] = Fix(Multiply(x, y))
  def add(x: Fix[Expr], y: Fix[Expr]): Fix[Expr] = Fix(Add(x, y))
}

class ExprSpec extends Specification {
  import Expr._

  val expr: Fix[Expr] = add(lit(5), multiply(lit(2), lit(4)))

  "should evaluate" >> {
    expr.cata(evaluate) should ===(13)
  }

  "should show" >> {
    expr.cata(show) must beEqualTo("(5 + (2 * 4))")
  }
}
