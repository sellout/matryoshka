/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.data

import slamdata.Predef.{Eq => _, _}
import turtles._

import cats._
import cats.data._

trait OptionInstances {
  implicit def optionSteppable[A]
      : Steppable.Aux[Option[A], Const[Option[A], ?]] =
    id.idSteppable[Option[A]]

  implicit def optionRecursive[A]
      : Recursive.Aux[Option[A], Const[Option[A], ?]] =
    id.idRecursive[Option[A]]

  implicit def optionCorecursive[A]
      : Corecursive.Aux[Option[A], Const[Option[A], ?]] =
    id.idCorecursive[Option[A]]

  implicit val optionDelayEq: Delay[Eq, Option] =
    new Delay[Eq, Option] {
      def apply[A](a: Eq[A]) = {
        implicit val aʹ: Eq[A] = a
        Eq[Option[A]]
      }
    }

  implicit val optionDelayOrder: Delay[Order, Option] =
    new Delay[Order, Option] {
      def apply[A](a: Order[A]) = {
        implicit val aʹ: Order[A] = a
        Order[Option[A]]
      }
    }

  implicit val optionDelayShow: Delay[Show, Option] =
    new Delay[Show, Option] {
      def apply[A](a: Show[A]) = {
        implicit val aʹ: Show[A] = a
        Show[Option[A]]
      }
    }
}

object option extends OptionInstances
