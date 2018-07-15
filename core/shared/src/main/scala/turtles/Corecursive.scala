/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

import slamdata.Predef.{Eq => _, _}
import turtles.derived._

import cats._
import cats.free._
import cats.implicits._

/** Unfolds for corecursive data types. */
trait Corecursive[T] extends Based[T] { self =>
  implicit val corec: Corecursive.Aux[T, Base] = self

  def embed(t: Base[T])(implicit BF: Functor[Base]): T

  def ana[A](a: A)(f: Coalgebra[Base, A])(implicit BF: Functor[Base]): T =
    hylo(a)(embed, f)

  def anaM[M[_]: Monad, A]
    (a: A)
    (f: CoalgebraM[M, Base, A])
    (implicit BT: Traverse[Base])
      : M[T] =
    hyloM[M, Base, A, T](a)(embed(_).pure[M], f)

  def gana[N[_]: Monad, A]
    (a: A)
    (k: DistributiveLaw[N, Base], f: GCoalgebra[N, Base, A])
    (implicit BF: Functor[Base])
      : T =
    ana[N[A]](a.pure[N])(na => k(na.map(f)).map(_.flatten))

  def ganaM[N[_]: Monad: Traverse, M[_]: Monad, A](
    a: A)(
    k: DistributiveLaw[N, Base], f: GCoalgebraM[N, M, Base, A])(
    implicit BT: Traverse[Base]):
      M[T] =
    ghyloM[Id, N, M, Base, A, T](a)(distCata, k, embed(_).pure[M], f)

  def elgotAna[N[_]: Monad, A](
    a: A)(
    k: DistributiveLaw[N, Base], ψ: ElgotCoalgebra[N, Base, A])(
    implicit BF: Functor[Base])
      : T =
    ana(ψ(a))(k(_).map(_ >>= ψ))

  /** An unfold that can short-circuit certain sections.
    */
  def apo[A](a: A)(f: GCoalgebra[Either[T, ?], Base, A])(implicit BF: Functor[Base])
      : T =
    hylo[λ[α => Base[Either[T, α]]], A, T](
      a)(
      fa => embed(fa.map(_.merge)), f)(
      BF.compose[Either[T, ?]])

  def elgotApo[A]
    (a: A)
    (f: ElgotCoalgebra[Either[T, ?], Base, A])
    (implicit BF: Functor[Base])
      : T =
    hylo[λ[α => Either[T, Base[α]]], A, T](
      a)(
      _.map(embed).merge, f)(
      Functor[Either[T, ?]] compose BF)

  /** An unfold that can handle sections with a secondary unfold.
    */
  def gapo[A, B]
    (a: A)
    (ψ0: Coalgebra[Base, B], ψ: GCoalgebra[Either[B, ?], Base, A])
    (implicit BF: Functor[Base])
      : T =
    hylo[λ[α => Base[Either[B, α]]], A, T](
      a)(
      fa => embed(fa.map(_.leftMap(ana(_)(ψ0)).merge)), ψ)(
      BF.compose[Either[B, ?]])

  def apoM[M[_]: Monad, A](
    a: A)(
    f: GCoalgebraM[Either[T, ?], M, Base, A])(
    implicit BT: Traverse[Base]):
      M[T] =
    hyloM[M, λ[α => Base[Either[T, α]]], A, T](
      a)(
      fa => embed(fa.map(_.merge)).pure[M], f)(
      Monad[M], BT.compose[Either[T, ?]])

  def futu[A]
    (a: A)
    (f: GCoalgebra[Free[Base, ?], Base, A])
    (implicit BF: Functor[Base])
      : T =
    gana[Free[Base, ?], A](a)(distFutu, f)

  def elgotFutu[A]
    (a: A)
    (f: ElgotCoalgebra[Free[Base, ?], Base, A])
    (implicit BF: Functor[Base])
      : T =
    elgotAna[Free[Base, ?], A](a)(distFutu, f)

  def futuM[M[_]: Monad, A](a: A)(f: GCoalgebraM[Free[Base, ?], M, Base, A])(
    implicit BT: Traverse[Base]):
      M[T] =
    ganaM[Free[Base, ?], M, A](a)(distFutu, f)

  def transAna[U, G[_]: Functor]
    (u: U)
    (f: G[U] => Base[U])
    (implicit U: Recursive.Aux[U, G], BF: Functor[Base])
      : T =
    ana(u)(f <<< (U.project(_)))

  def transGana[M[_]: Monad, U, G[_]: Functor]
    (u: U)
    (k: DistributiveLaw[M, Base], f: CoalgebraicGTransform[M, U, G, Base])
    (implicit U: Recursive.Aux[U, G], BF: Functor[Base])
      : T =
    gana(u)(k, f <<< (U.project(_)))

  def transApo[U, G[_]: Functor]
    (u: U)
    (f: CoalgebraicGTransform[Either[T, ?], U, G, Base])
    (implicit U: Recursive.Aux[U, G], BF: Functor[Base])
      : T = {
    implicit val nested: Functor[λ[α => Base[Either[T, α]]]] =
      BF.compose[Either[T, ?]]

    transHylo[U, G, λ[α => Base[Either[T, α]]], T, Base](u)(_.map(_.merge), f)
  }

  def transFutu[U, G[_]: Functor]
    (u: U)
    (f: CoalgebraicGTransform[Free[Base, ?], U, G, Base])
    (implicit U: Recursive.Aux[U, G], BF: Functor[Base])
      : T =
    transGana(u)(distFutu[Base], f)

  def transAnaM[M[_]: Monad, U, G[_]: Functor]
    (u: U)
    (f: TransformM[M, U, G, Base])
    (implicit U: Recursive.Aux[U, G], BT: Traverse[Base])
      : M[T] =
    anaM(u)(f <<< (U.project(_)))
}

object Corecursive {
  def fromAlgebra[T, F[_]](φ: Algebra[F, T]): Aux[T, F] = new Corecursive[T] {
    type Base[A] = F[A]
    def embed(ft: F[T])(implicit BF: Functor[Base]) = φ(ft)
  }

  type Aux[T, F[_]] = Corecursive[T] { type Base[A] = F[A] }

  def apply[T](implicit instance: Corecursive[T]): Aux[T, instance.Base] =
    instance
}
