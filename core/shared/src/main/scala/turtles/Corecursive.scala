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

  def ana[A](a: A)(f: Coalgebra[Base, A]): T

  def anaM[M[_]: Monad, A]
    (a: A)
    (f: CoalgebraM[M, Base, A])
    (implicit T: Steppable.Aux[T, Base], BT: Traverse[Base])
      : M[T] =
    hyloM[M, Base, A, T](a)(T.embed(_).pure[M], f)

  def gana[N[_]: Monad, A]
    (a: A)
    (k: DistributiveLaw[N, Base], f: GCoalgebra[N, Base, A])
    (implicit BF: Functor[Base])
      : T =
    ana[N[A]](a.pure[N])(na => k(na.map(f)).map(_.flatten))

  def ganaM[N[_]: Monad: Traverse, M[_]: Monad, A](
    a: A)(
    k: DistributiveLaw[N, Base], f: GCoalgebraM[N, M, Base, A])(
    implicit T: Steppable.Aux[T, Base], BT: Traverse[Base]):
      M[T] =
    ghyloM[Id, N, M, Base, A, T](a)(distCata, k, T.embed(_).pure[M], f)

  def elgotAna[N[_]: Monad, A](
    a: A)(
    k: DistributiveLaw[N, Base], ψ: ElgotCoalgebra[N, Base, A])(
    implicit BF: Functor[Base])
      : T =
    ana(ψ(a))(k(_).map(_ >>= ψ))

  /** An unfold that can short-circuit certain sections.
    */
  def apo[A]
    (a: A)
    (f: GCoalgebra[Either[T, ?], Base, A])
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : T =
    gana[Either[T, ?], A](a)(distApo, f)

  def elgotApo[A]
    (a: A)
    (f: ElgotCoalgebra[Either[T, ?], Base, A])
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : T =
    elgotAna[Either[T, ?], A](a)(distApo, f)

  /** An unfold that can handle sections with a secondary unfold.
    */
  def gapo[A, B]
    (a: A)
    (ψ0: Coalgebra[Base, B], ψ: GCoalgebra[Either[B, ?], Base, A])
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : T =
    hylo[λ[α => Base[Either[B, α]]], A, T](
      a)(
      fa => T.embed(fa.map(_.leftMap(ana(_)(ψ0)).merge)), ψ)(
      BF.compose[Either[B, ?]])

  def apoM[M[_]: Monad, A](
    a: A)(
    f: GCoalgebraM[Either[T, ?], M, Base, A])(
    implicit T: Steppable.Aux[T, Base], BT: Traverse[Base]):
      M[T] =
    hyloM[M, λ[α => Base[Either[T, α]]], A, T](
      a)(
      fa => T.embed(fa.map(_.merge)).pure[M], f)(
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
    implicit T: Steppable.Aux[T, Base], BT: Traverse[Base]):
      M[T] =
    ganaM[Free[Base, ?], M, A](a)(distFutu, f)

  def postpro[A](
    a: A)(
    e: Base ~> Base, g: Coalgebra[Base, A])(
    implicit T: Steppable.Aux[T, Base], BF: Functor[Base]):
      T =
    gpostpro[Id, A](a)(distAna, e, g)

  def gpostpro[N[_], A](
    a: A)(
    k: DistributiveLaw[N, Base], e: Base ~> Base, ψ: GCoalgebra[N, Base, A])(
    implicit T: Steppable.Aux[T, Base], BF: Functor[Base], N: Monad[N]):
      T =
    hylo[Yoneda[Base, ?], N[A], T](
      a.pure[N])(
      fa => T.embed((fa.map(ana(_)(x => e(T.project(x))))).run),
        ma => Yoneda(k(ma.map(ψ))).map(_.flatten))

  def transAna[U, G[_]: Functor]
    (u: U)
    (f: G[U] => Base[U])
    (implicit U: Steppable.Aux[U, G], BF: Functor[Base])
      : T =
    ana(u)(f <<< (U.project(_)))

  def transGana[M[_]: Monad, U, G[_]: Functor]
    (u: U)
    (k: DistributiveLaw[M, Base], f: CoalgebraicGTransform[M, U, G, Base])
    (implicit U: Steppable.Aux[U, G], BF: Functor[Base])
      : T =
    gana(u)(k, f <<< (U.project(_)))

  def transApo[U, G[_]: Functor]
    (u: U)
    (f: CoalgebraicGTransform[Either[T, ?], U, G, Base])
    (implicit
      T: Steppable.Aux[T, Base],
      U: Steppable.Aux[U, G],
      BF: Functor[Base])
      : T =
    transGana(u)(distApo, f)

  def transFutu[U, G[_]: Functor]
    (u: U)
    (f: CoalgebraicGTransform[Free[Base, ?], U, G, Base])
    (implicit U: Steppable.Aux[U, G], BF: Functor[Base])
      : T =
    transGana(u)(distFutu[Base], f)

  def transAnaM[M[_]: Monad, U, G[_]: Functor]
    (u: U)
    (f: TransformM[M, U, G, Base])
    (implicit
      T: Steppable.Aux[T, Base],
      U: Steppable.Aux[U, G],
      BT: Traverse[Base])
      : M[T] =
    anaM(u)(f <<< (U.project(_)))

  def transAnaT
    (t: T)
    (f: T => T)
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : T =
    ana(t)(f >>> T.project)

  /** This behaves like [[turtles.Corecursive.elgotApo]]`, but it’s harder to
    * see from the types that in the disjunction, `Left` is the final result for
    * this node, while `Right` means to keep processing the children.
    */
  def transApoT
    (t: T)
    (f: T => Either[T, T])
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : T =
    elgotApo(t)(f(_).map(T.project))

  def transAnaTM[M[_]: Monad]
    (t: T)
    (f: T => M[T])
    (implicit T: Steppable.Aux[T, Base], BF: Traverse[Base])
      : M[T] =
    anaM(t)(f(_).map(T.project))

  // TODO: This should be an enrichment on Tuple2.
  private def sequenceTuple[F[_]: Functor, A, B](tup: (A, F[B])): F[(A, B)] =
    tup._2.map((tup._1, _))

  def topDownCata[A]
    (t: T, a: A)
    (f: (A, T) => (A, T))
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : T =
    ana((a, t))(at => sequenceTuple(f.tupled(at).map(T.project)))

  def topDownCataM[M[_]: Monad, A](
    t: T, a: A)(
    f: (A, T) => M[(A, T)])(
    implicit T: Steppable.Aux[T, Base], BT: Traverse[Base]):
      M[T] =
    anaM((a, t))(f.tupled(_).map(aft => sequenceTuple(aft.map(T.project))))
}

object Corecursive {
  /** Create a [[Corecursive]] instance from the mappings to/from the
    * fixed-point.
    */
  def withNativeRecursion[T, F[_]]
    (implicit T: Steppable.Aux[T, F], F: Functor[F])
      : Corecursive.Aux[T, F] =
    new Corecursive[T] {
      type Base[A] = F[A]
      def ana[A](a: A)(f: Coalgebra[Base, A]) = hylo(a)(T.embed, f)
    }

  def corecursiveEq[T, F[_]: Traverse]
    (implicit
      TS: Steppable.Aux[T, F],
      TC: Corecursive.Aux[T, F],
      F: Eq[F[Unit]])
      : Eq[T] =
    Eq.instance((a, b) =>
      TC.anaM[Option, (T, T)]((a, b)) {
        case (a, b) => Merge.fromTraverse[F].merge(TS.project(a), TS.project(b))
      }.isDefined)

  def order[T, F[_]: Traverse]
    (implicit
      TS: Steppable.Aux[T, F],
      TC: Corecursive.Aux[T, F],
      F: Order[F[Unit]])
      : Order[T] =
    Order.from((a, b) =>
      TC.anaM[Either[Int, ?], (T, T)]((a, b)) {
        case (a, b) =>
          val (fa, fb) = (TS.project(a), TS.project(b))
          Merge.fromTraverse[F].merge(fa, fb).toRight(fa.void compare fb.void)
      }.as(0).merge)

  type Aux[T, F[_]] = Corecursive[T] { type Base[A] = F[A] }

  def apply[T](implicit instance: Corecursive[T]): Aux[T, instance.Base] =
    instance
}
