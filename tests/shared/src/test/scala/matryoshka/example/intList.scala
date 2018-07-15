/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package matryoshka.example

import slamdata.Predef._
import turtles._
import turtles.data._
import turtles.implicits._

import scala.Either

import cats._
import cats.implicits._
import org.specs2.mutable._

sealed trait IntList[A]

object IntList {

  case class IntCons[A](h: Int, tail: A) extends IntList[A]
  case class IntNil[A]() extends IntList[A]

  implicit val intListFunctor: Functor[IntList] = new Functor[IntList] {
    def map[A, B](list: IntList[A])(f: A => B): IntList[B] = list match {
      case IntNil() => IntNil()
      case IntCons(h, t) => IntCons(h, f(t))
    }
  }

  val to: Coalgebra[IntList, List[Int]] = {
    case Nil => IntNil()
    case h :: t => IntCons(h, t)
  }

  val from: Algebra[IntList, List[Int]] = {
    case IntNil() => Nil
    case IntCons(h, t) => h :: t
  }

  val sum: Algebra[IntList, Int] = {
    case IntNil() => 0
    case IntCons(h, t) => h + t
  }

  val len: Algebra[IntList, Int] = {
    case IntNil() => 0
    case IntCons(_, t) => t + 1
  }

  def filter(f: Int => Boolean): Algebra[IntList, List[Int]] = {
    case IntNil() => Nil
    case IntCons(h, t) => if(f(h)) h :: t else t
  }

  def lessThan(i: Int): IntList ~> IntList = new (IntList ~> IntList) {
    def apply[A](l: IntList[A]): IntList[A] = l match {
      case IntNil() => IntNil()
      case l @ IntCons(h, t) => if(h < i) l else IntNil()
    }
  }

  def mapHead(f: Int => Int): GCoalgebra[Either[Fix[IntList], ?], IntList, Fix[IntList]] = {
    case Fix(IntNil()) => IntNil()
    case Fix(IntCons(h, t)) => IntCons(f(h), t.asLeft)
  }

  val infinite: Coalgebra[IntList, Int] = n => IntCons(n, n + 1)
}

class IntListSpec extends Specification {
  import IntList._

  val intList = Fix(IntCons(1, Fix(IntCons(2, Fix(IntNil[Fix[IntList]]())))))

  "construct an IntList" >> {
    List(1, 2).ana[Fix[IntList]](to) should ===(intList)
  }
  "convert an IntList to a List" >> {
    intList.cata(from) should ===(List(1, 2))
  }

  "filter an IntList" >> {
    (0 until 10).toList.hylo(filter(_ < 5), to) should ===((0 until 5).toList)
  }

  "map the head of an IntList" >> {
    intList.apo.apply(mapHead(_ * 3)).cata(from) should ===(List(3, 2))
  }

  // "short circuit the creation of infinite IntList" >> {
  //   1.postpro[Fix[IntList]](IntList.lessThan(10), infinite).cata(len) should ===(9)
  // }

  "short circuit the fold of an IntList" >> {
    intList.prepro(IntList.lessThan(2), sum) should ===(1)
  }
}
