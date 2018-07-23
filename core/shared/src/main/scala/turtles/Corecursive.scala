/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

import slamdata.Predef.{Eq => _, _}
import turtles.derived._

import cats._
import cats.data._
import cats.free._
import cats.implicits._

/** Unfolds for corecursive data types. */
trait Corecursive[T] extends Based[T] { self =>
  implicit val corec: Corecursive.Aux[T, Base] = self

  /** The fundamental operation on all potentially infinite data types. */
  def ana[A](a: A)(ψ: Coalgebra[Base, A]): T

  /** A generalization of [[ana]] that allows an arbitrary [[cats.Monad]] to
    * modify the carrier, so long as [[Base]] can be distributed over it.
    */
  def gana[N[_]: Monad, A]
    (a: A)
    (k: DistributiveLaw[N, Base], ψ: GCoalgebra[N, Base, A])
    (implicit BF: Functor[Base])
      : T =
    ana[N[A]](a.pure[N])(na => k(na.map(ψ)).map(_.flatten))

  /** @see [[gana]]. */
  def elgotAna[N[_]: Monad, A]
    (a: A)
    (k: DistributiveLaw[N, Base], ψ: ElgotCoalgebra[N, Base, A])
    (implicit BF: Functor[Base])
      : T =
    ana(ψ(a))(k(_).map(_ >>= ψ))

  /** An unfold that can “short-circuit” certain sections by choosing to return
    * a complete branch (using [[scala.Left]]) instead of single step (using
    * [[scala.Right]]).
    */
  def apo[A]
    (a: A)
    (ψ: GCoalgebra[Either[T, ?], Base, A])
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : T =
    gana[Either[T, ?], A](a)(distApo, ψ)

  /** @see [[apo]]. */
  def elgotApo[A]
    (a: A)
    (ψ: ElgotCoalgebra[Either[T, ?], Base, A])
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : T =
    elgotAna[Either[T, ?], A](a)(distApo, ψ)

  /** An unfold that can handle sections with a secondary unfold. This is a
    * generalization of [[apo]] that allows applying a different unfold when
    * [[scala.Left]] is used. */
  def gapo[A, B]
    (a: A)
    (ψʹ: Coalgebra[Base, B], ψ: GCoalgebra[Either[B, ?], Base, A])
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : T =
    gana[Either[B, ?], A](a)(distGApo(ψʹ), ψ)

  /** @see [[gapo]]. */
  def gapoT[M[_]: Monad, A, B]
    (a: A)
    (ψʹ: Coalgebra[Base, B],
      k: DistributiveLaw[M, Base],
      ψ: GCoalgebra[EitherT[M, B, ?], Base, A])
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : T =
    gana[EitherT[M, B, ?], A](a)(distGApoT(ψʹ, k), ψ)

  /** An unfold that can emit multiple steps at a time.
    *
    * NB: If your only cases are returning either a single step or a complete
    *     branch, @see [[apo]] instead.
    */
  def futu[A]
    (a: A)
    (ψ: GCoalgebra[Free[Base, ?], Base, A])
    (implicit BF: Functor[Base])
      : T =
    gana[Free[Base, ?], A](a)(distFutu, ψ)

  /** @see [[futu]]. */
  def futuT[H[_]: Monad, A](a: A)
    (k: DistributiveLaw[H, Base], ψ: GCoalgebra[FreeT[Base, H, ?], Base, A])
    (implicit BF: Functor[Base])
      : T =
    gana[FreeT[Base, H, ?], A](a)(distFutuT(k), ψ)

  /** @see [[futu]]. */
  def elgotFutu[A]
    (a: A)
    (ψ: ElgotCoalgebra[Free[Base, ?], Base, A])
    (implicit BF: Functor[Base])
      : T =
    elgotAna[Free[Base, ?], A](a)(distFutu, ψ)

  /** This is impossible to define in a safe manner. Bringing the `M` to the top
    * of the fixed point necessarily traverses the entire (potentially-infinite)
    * structure. If you are reaching for this (or anything defined in terms of
    * it), you probably want to look at a streaming library instead. Or, even
    * better, look for alternative total approaches.
    */
  def anaM[M[_]: Monad, A]
    (a: A)
    (ψ: CoalgebraM[M, Base, A])
    (implicit T: Steppable.Aux[T, Base], BT: Traverse[Base])
      : M[T] =
    hyloM[M, Base, A, T](a)(T.embed(_).pure[M], ψ)

  /** @see [[anaM]]. */
  def ganaM[N[_]: Monad: Traverse, M[_]: Monad, A]
    (a: A)
    (k: DistributiveLaw[N, Base], ψ: GCoalgebraM[N, M, Base, A])
    (implicit T: Steppable.Aux[T, Base], BT: Traverse[Base])
      : M[T] =
    anaM[M, N[A]](a.pure[N])(_.traverse(ψ).map(k(_).map(_.flatten)))

  /** @see [[anaM]]. */
  def apoM[M[_]: Monad, A]
    (a: A)
    (ψ: GCoalgebraM[Either[T, ?], M, Base, A])
    (implicit T: Steppable.Aux[T, Base], BT: Traverse[Base])
      : M[T] =
    ganaM[Either[T, ?], M, A](a)(distApo, ψ)

  /** @see [[anaM]]. */
  def futuM[M[_]: Monad, A]
    (a: A)
    (ψ: GCoalgebraM[Free[Base, ?], M, Base, A])
    (implicit T: Steppable.Aux[T, Base], BT: Traverse[Base])
      : M[T] =
    ganaM[Free[Base, ?], M, A](a)(distFutu, ψ)

  def postpro[A]
    (a: A)
    (e: Base ~> Base, ψ: Coalgebra[Base, A])
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : T =
    gpostpro[Id, A](a)(distAna, e, ψ)

  def gpostpro[N[_], A]
    (a: A)
    (k: DistributiveLaw[N, Base], e: Base ~> Base, ψ: GCoalgebra[N, Base, A])
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base], N: Monad[N])
      : T =
    ghylo[Id, N, Base, A, T](
      a)(
      distCata,
        k,
        BF.lift[T, T](ana[T](_)(x => e(T.project(x)))) >>> T.embed,
        ψ)

  /** Converts from any `Steppable` type to this type. I.e., you can
    * expand any fixed pount to the greatest fixed point.
    *
    * This is normally unnecessary, unless some function you are passing
    * it to is insufficiently generalized.
    */
  def convertFrom[R](r: R)(implicit R: Steppable.Aux[R, Base]): T =
    ana[R](r)(R.project)

  /** @see [[ana]]. */
  def transAna[U, G[_]: Functor]
    (u: U)
    (f: G[U] => Base[U])
    (implicit U: Steppable.Aux[U, G], BF: Functor[Base])
      : T =
    ana(u)(f <<< (U.project(_)))

  /** @see [[gana]]. */
  def transGana[M[_]: Monad, U, G[_]: Functor]
    (u: U)
    (k: DistributiveLaw[M, Base], f: CoalgebraicGTransform[M, U, G, Base])
    (implicit U: Steppable.Aux[U, G], BF: Functor[Base])
      : T =
    gana(u)(k, f <<< (U.project(_)))

  /** @see [[apo]]. */
  def transApo[U, G[_]: Functor]
    (u: U)
    (f: CoalgebraicGTransform[Either[T, ?], U, G, Base])
    (implicit
      T: Steppable.Aux[T, Base],
      U: Steppable.Aux[U, G],
      BF: Functor[Base])
      : T =
    transGana(u)(distApo, f)

  /** @see [[futu]]. */
  def transFutu[U, G[_]: Functor]
    (u: U)
    (f: CoalgebraicGTransform[Free[Base, ?], U, G, Base])
    (implicit U: Steppable.Aux[U, G], BF: Functor[Base])
      : T =
    transGana(u)(distFutu[Base], f)

  /** @see [[anaM]]. */
  def transAnaM[M[_]: Monad, U, G[_]: Functor]
    (u: U)
    (f: TransformM[M, U, G, Base])
    (implicit
      T: Steppable.Aux[T, Base],
      U: Steppable.Aux[U, G],
      BT: Traverse[Base])
      : M[T] =
    anaM(u)(f <<< (U.project(_)))

  /** @see [[ana]]. */
  def transAnaT
    (t: T)
    (f: T => T)
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : T =
    ana(t)(f >>> T.project)

  /** This behaves like [[elgotApo]], but it’s harder to see from the types that
    * in the disjunction, [[scala.Left]] is the final result for this node,
    * while [[scala.Right]] means to keep processing the children.
    */
  def transApoT
    (t: T)
    (f: T => Either[T, T])
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : T =
    elgotApo(t)(f(_).map(T.project))

  /** @see [[anaM]]. */
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

  /** @see [[anaM]]. */
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
