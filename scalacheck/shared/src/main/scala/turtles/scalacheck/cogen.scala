/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.scalacheck

import slamdata.Predef.{Eq => _, _}
import turtles._
import turtles.data._
import turtles.implicits._
import turtles.patterns._

import cats._
import cats.free._
import org.scalacheck._

trait CogenInstancesʹ {
  implicit def delayCogen[F[_], A](implicit F: Delay[Cogen, F], A: Cogen[A]): Cogen[F[A]] =
    F(A)
}

trait CogenInstances extends CogenInstancesʹ {
  // TODO: Define this using a fold rather than `project`
  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def steppableCogen[T, F[_]: Functor]
    (implicit T: Steppable.Aux[T, F], F: Delay[Cogen, F])
      : Cogen[T] =
    Cogen((seed, value) => F(steppableCogen[T, F]).perturb(seed, value.project))

  implicit def fixCogen[F[_]: Functor](implicit F: Delay[Cogen, F]): Cogen[Fix[F]] =
    steppableCogen[Fix[F], F]
  implicit def muCogen[F[_]: Functor](implicit F: Delay[Cogen, F]): Cogen[Mu[F]] =
    steppableCogen[Mu[F], F]
  implicit def nuCogen[F[_]: Functor](implicit F: Delay[Cogen, F]): Cogen[Nu[F]] =
    steppableCogen[Nu[F], F]

  implicit val optionCogen: Delay[Cogen, Option] =
    new Delay[Cogen, Option] {
      def apply[A](a: Cogen[A]) =
        Cogen((seed, value) => value.fold(seed.next)(a.perturb(seed, _)))
    }

  implicit def coEnvCogen[F[_], A: Cogen](implicit F: Delay[Cogen, F]): Delay[Cogen, CoEnv[A, F, ?]] =
    new Delay[Cogen, CoEnv[A, F, ?]] {
      def apply[B](b: Cogen[B]) =
        Cogen((seed, value) => value.run.fold(Cogen[A].perturb(seed, _), F(b).perturb(seed.next, _)))
    }

  implicit def listFCogen[A: Cogen]: Delay[Cogen, ListF[A, ?]] =
    new Delay[Cogen, ListF[A, ?]] {
      def apply[B](b: Cogen[B]) =
        Cogen((seed, value) => value match {
          case ConsF(h, t) => b.perturb(Cogen[A].perturb(seed, h), t)
          case NilF()      => seed
        })
    }

  implicit def envTCogen[F[_], A: Cogen](implicit F: Delay[Cogen, F]): Delay[Cogen, EnvT[A, F, ?]] =
    new Delay[Cogen, EnvT[A, F, ?]] {
      def apply[B](b: Cogen[B]) =
        Cogen((seed, value) => F(b).perturb(Cogen[A].perturb(seed, value.ask), value.lower))
    }

  implicit def freeCogen[F[_]: Functor](implicit F: Delay[Cogen, F]): Delay[Cogen, Free[F, ?]] =
    new Delay[Cogen, Free[F, ?]] {
      def apply[A](a: Cogen[A]) = {
        implicit val aʹ: Cogen[A] = a
        steppableCogen[Free[F, A], CoEnv[A, F, ?]]
      }
    }

  implicit def cofreeCogen[F[_]: Functor](implicit F: Delay[Cogen, F]): Delay[Cogen, Cofree[F, ?]] =
    new Delay[Cogen, Cofree[F, ?]] {
      def apply[A](a: Cogen[A]) = {
        implicit val aʹ: Cogen[A] = a
        steppableCogen[Cofree[F, A], EnvT[A, F, ?]]
      }
    }
}

package object cogen extends CogenInstances
