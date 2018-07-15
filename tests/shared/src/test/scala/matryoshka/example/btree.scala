/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package matryoshka.example

import slamdata.Predef._
import turtles._
import turtles.implicits._

import cats._
import org.specs2.mutable._

sealed trait BTree[A]

object BTree {

  case class BTNil[A]() extends BTree[A]
  case class Leaf[A](value: Int) extends BTree[A]
  case class Node[A](l: A , r: A) extends BTree[A]

  implicit val treeFunctor: Functor[BTree] = new Functor[BTree] {
    def map[A, B](tree: BTree[A])(f: A => B): BTree[B] = tree match {
      case BTNil() => BTNil()
      case Leaf(v) => Leaf(v)
      case Node(l, r) => Node(f(l), f(r))
    }
  }

  def merge(l: List[Int], r: List[Int]): List[Int] =
    (l, r) match {
	  case(l, Nil) => l
	  case(Nil, r) => r
	  case(lh :: ls, rh :: rs) =>
	    if (lh < rh) lh::merge(ls, r)
            else rh :: merge(l, rs)
    }

  //builds a balanced binary tree
  val to: Coalgebra[BTree, List[Int]] = {
    case Nil => BTNil()
    case x :: Nil => Leaf(x)
    case xs => val (l, r) = xs.splitAt(xs.length / 2)
      Node(l, r)
  }

  //sorts a balanced binary tree
  val mergeSort: Algebra[BTree, List[Int]] = {
    case BTNil() => Nil
    case Leaf(v) => v :: Nil
    case Node(l, r) => merge(l, r)
  }
}

class BTreeSpec extends Specification {
  import BTree._

  "should sort a list" >> {
    val list = List(23, 565, 6, 23, 45, 25, 678, 5)
    val sorted = list.hylo(mergeSort, to)
    sorted should ===(list.sorted)
  }
}
