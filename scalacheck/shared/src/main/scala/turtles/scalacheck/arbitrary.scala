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
import cats.data._
import cats.free._
import cats.implicits._
import org.scalacheck._
import org.scalacheck.support.cats._

trait ArbitraryInstancesʹ {
  implicit def delayArbitrary[F[_], A](
    implicit A: Arbitrary[A], F: Delay[Arbitrary, F]):
      Arbitrary[F[A]] =
    F(A)
}

trait ArbitraryInstances extends ArbitraryInstancesʹ {
  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def steppableArbitrary[T, F[_]: Functor]
    (implicit T: Steppable.Aux[T, F], fArb: Delay[Arbitrary, F])
      : Arbitrary[T] =
    Arbitrary(Gen.sized(size =>
      fArb(Arbitrary(
        if (size <= 0)
          Gen.fail[T]
        else
          Gen.resize(size - 1, steppableArbitrary[T, F].arbitrary))).arbitrary.map(_.embed)))

  implicit def fixArbitrary[F[_]: Functor](implicit fArb: Delay[Arbitrary, F]): Arbitrary[Fix[F]] =
    steppableArbitrary[Fix[F], F]

  implicit def muArbitrary[F[_]: Functor](implicit fArb: Delay[Arbitrary, F]): Arbitrary[Mu[F]] =
    steppableArbitrary[Mu[F], F]

  implicit def nuArbitrary[F[_]: Functor](implicit fArb: Delay[Arbitrary, F]): Arbitrary[Nu[F]] =
    steppableArbitrary[Nu[F], F]

  implicit def coEnvArbitrary[E: Arbitrary, F[_]](
    implicit F: Delay[Arbitrary, F]):
      Delay[Arbitrary, CoEnv[E, F, ?]] =
    new Delay[Arbitrary, CoEnv[E, F, ?]] {
      def apply[α](arb: Arbitrary[α]) = {
        implicit val a = arb

        Arbitrary(Arbitrary.arbitrary[Either[E, F[α]]].map(CoEnv(_)))
      }
    }

  implicit def envTArbitrary[E: Arbitrary, F[_]](implicit F: Delay[Arbitrary, F]): Delay[Arbitrary, EnvT[E, F, ?]] =
    new Delay[Arbitrary, EnvT[E, F, ?]] {
      def apply[A](arb: Arbitrary[A]) =
        Arbitrary(
          (Arbitrary.arbitrary[E], F(arb).arbitrary).mapN((e, f) => EnvT((e, f))))
    }

  implicit def listFArbitrary[A: Arbitrary]: Delay[Arbitrary, ListF[A, ?]] =
    new Delay[Arbitrary, ListF[A, ?]] {
      def apply[B](arb: Arbitrary[B]) =
        Arbitrary(Gen.oneOf[ListF[A, B]](
          NilF[A, B]().pure[Gen],
          (Arbitrary.arbitrary[A], arb.arbitrary).mapN(ConsF[A, B])))
    }

  implicit def nelFArbitrary[A: Arbitrary]: Delay[Arbitrary, AndMaybe[A, ?]] =
    new Delay[Arbitrary, AndMaybe[A, ?]] {
      def apply[B](arb: Arbitrary[B]) =
        Arbitrary(Gen.oneOf[AndMaybe[A, B]](
          (Arbitrary.arbitrary[A], arb.arbitrary).mapN(Indeed[A, B]),
          Arbitrary.arbitrary[A].map(Only[A, B](_))))
    }

  implicit def cofreeArbitrary[F[_]: Functor, A]
    (implicit F: Delay[Arbitrary, F], A: Arbitrary[A])
      : Arbitrary[Cofree[F, A]] =
    steppableArbitrary[Cofree[F, A], EnvT[A, F, ?]]

  implicit def freeArbitrary[F[_]: Functor, A]
    (implicit F: Delay[Arbitrary, F], A: Arbitrary[A])
      : Arbitrary[Free[F, A]] =
    steppableArbitrary[Free[F, A], CoEnv[A, F, ?]]

  implicit val optionArbitrary: Delay[Arbitrary, Option] =
    new Delay[Arbitrary, Option] {
      def apply[A](arb: Arbitrary[A]) =
        Arbitrary(Gen.frequency(
          ( 1, None.pure[Gen]),
          (50, arb.arbitrary.map(_.some))))
    }

  implicit def eitherArbitrary[A: Arbitrary]: Delay[Arbitrary, Either[A, ?]] =
    new Delay[Arbitrary, Either[A, ?]] {
      def apply[B](arb: Arbitrary[B]) =
        Arbitrary(Gen.oneOf(
          Arbitrary.arbitrary[A].map(Left(_)),
          arb.arbitrary.map(Right(_))))
    }

  implicit def nonEmptyListArbitrary: Delay[Arbitrary, NonEmptyList] =
    new Delay[Arbitrary, NonEmptyList] {
      def apply[A](arb: Arbitrary[A]) =
        Arbitrary((arb.arbitrary, Gen.listOf[A](arb.arbitrary)).mapN(
          NonEmptyList(_, _)))
    }
}

package object arbitrary extends ArbitraryInstances
