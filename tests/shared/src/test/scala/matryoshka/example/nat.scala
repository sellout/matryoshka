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

sealed trait Nat[A]

object Nat {

  case class Zero[A]() extends Nat[A]
  case class Succ[A](n: A) extends Nat[A]

  implicit val natFunctor: Functor[Nat] = new Functor[Nat] {
    def map[A, B](nat: Nat[A])(f: A => B): Nat[B] = nat match {
      case Zero() => Zero()
      case Succ(a) => Succ(f(a))
    }
  }

  val toNat: Coalgebra[Nat, Int] = {
    case 0 => Zero()
    case n => Succ(n - 1)  
  }

  val toInt: Algebra[Nat, Int] = {
    case Zero() => 0
    case Succ(n) => n + 1
  }

  val factorial: GAlgebra[(Int, ?), Nat, Int] = {
    case Zero() => 1
    case Succ((i, n)) => (i + 1) * n
  }
}

class NatSpec extends Specification {
  import Nat._

  val nat2 = Fix(Succ(Fix(Succ(Fix(Zero[Fix[Nat]]())))))

  "create a Nat from an Int" >> {
    2.ana[Fix[Nat]](toNat) should ===(nat2)
  }
  "create an Int from a Nat" >> {
    nat2.cata(toInt) should ===(2)
  }
  "convert an Int to a Nat and back" >> {
    2.hylo(toInt, toNat) should ===(2)
  }
  "calculate the factorial of a Nat" >> {
    val nat4 = 4.ana[Fix[Nat]](toNat)
    nat4.zygo(toInt, factorial) should ===((24))
  }
}
