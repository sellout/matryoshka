/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

import slamdata.Predef.{Eq => _, _}

import cats._
import cats.free._
import cats.implicits._

/** A type that is both [[Recursive]] and [[Corecursive]].
  */
trait Birecursive[T] extends Recursive[T] with Corecursive[T] {
  /** Roughly a default impl of `project`, given a [[turtles.Corecursive]]
    * instance and an overridden `cata`.
    */
  def lambek
    (tf: T)
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : Base[T] =
    cata[Base[T]](tf)(_.map(T.embed))

  /** Roughly a default impl of `embed`, given a [[turtles.Recursive]]
    * instance and an overridden `ana`.
    */
  def colambek
    (ft: Base[T])
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : T =
    ana(ft)(_.map(T.project))

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

  // FIXME: This should be an override, but it adds an extra implicit.
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def paraZygo[A, B]
    (t: T)
    (f: GAlgebra[(T, ?), Base, B], g: GAlgebra[(B, ?), Base, A])
    (implicit
      T: Steppable.Aux[T, Base],
      BF: Functor[Base],
      BU: Alternative[Base]) =
    gcataZygo[(T, ?), A, B](t)(distPara, f, g)

  def prepro[A]
    (t: T)
    (e: Base ~> Base, f: Algebra[Base, A])
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : A =
    gprepro[Id, A](t)(distCata, e, f)

  def gprepro[W[_]: Comonad, A](
    t: T)(
    k: DistributiveLaw[Base, W], e: Base ~> Base, f: GAlgebra[W, Base, A])(
    implicit T: Steppable.Aux[T, Base], BF: Functor[Base]):
      A =
    hylo[Yoneda[Base, ?], T, W[A]](
      t)(
      fwa => k((fwa.map(_.coflatten)).run).map(f),
        t => Yoneda(T.project(t)).map(cata[T](_)(c => T.embed(e(c))))).extract

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

  def transPrepro[U, G[_]: Functor]
    (t: T)
    (e: Base ~> Base, f: Transform[U, Base, G])
    (implicit
      T: Steppable.Aux[T, Base],
      U: Steppable.Aux[U, G],
      BF: Functor[Base])
      : U =
    prepro(t)(e, f >>> (U.embed(_)))

  def transCataT
    (t: T)
    (f: T => T)
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : T =
    cata(t)(f <<< T.embed)

  /** This behaves like [[turtles.Recursive.elgotPara]]`, but it’s harder to
    * see from the types that in the tuple, `_2` is the result so far and `_1`
    * is the original structure.
    */
  def transParaT
    (t: T)
    (f: ((T, T)) => T)
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : T =
    elgotPara[T](t)(f <<< (_.map(T.embed)))

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

  def transCataTM[M[_]: Monad]
    (t: T)
    (f: T => M[T])
    (implicit T: Steppable.Aux[T, Base], BF: Traverse[Base])
      : M[T] =
    cataM(t)(f <<< T.embed)

  def transAnaTM[M[_]: Monad]
    (t: T)
    (f: T => M[T])
    (implicit T: Steppable.Aux[T, Base], BF: Traverse[Base])
      : M[T] =
    anaM(t)(f(_).map(T.project))
}

object Birecursive {
  /** Create a [[Birecursive]] instance from the mappings to/from the
    * fixed-point.
    */
  def withNativeRecursion[T, F[_]]
    (implicit T: Steppable.Aux[T, F], F: Functor[F])
      : Birecursive.Aux[T, F] =
    new Birecursive[T] {
      type Base[A] = F[A]

      def ana[A](a: A)(f: Coalgebra[Base, A]) = hylo(a)(T.embed, f)
      def cata[A](t: T)(f: Algebra[Base, A]) = hylo(t)(f, T.project)
    }

  def lambekIso[T, F[_]: Functor]
    (implicit TS: Steppable.Aux[T, F], TB: Birecursive.Aux[T, F]) =
    AlgebraIso[F, T](TB.colambek(_))(TB.lambek(_))

  // NB: The rest of this is what would be generated by simulacrum, except this
  //     type class is too complicated to take advantage of that.

  type Aux[T, F[_]] = Birecursive[T] { type Base[A] = F[A] }

  def apply[T](implicit instance: Birecursive[T]): Aux[T, instance.Base] =
    instance

  trait Ops[T, F[_]] {
    def typeClassInstance: Aux[T, F]
    def self: T

    def lambek(implicit T: Steppable.Aux[T, F], BF: Functor[F]): F[T] =
      typeClassInstance.lambek(self)
    def prepro[A]
      (e: F ~> F, f: Algebra[F, A])
      (implicit T: Steppable.Aux[T, F], BF: Functor[F])
        : A =
      typeClassInstance.prepro[A](self)(e, f)
    def gprepro[W[_]: Comonad, A]
      (k: DistributiveLaw[F, W], e: F ~> F, f: GAlgebra[W, F, A])
      (implicit T: Steppable.Aux[T, F], BF: Functor[F])
        : A =
      typeClassInstance.gprepro[W, A](self)(k, e, f)
    def topDownCata[A]
      (a: A)
      (f: (A, T) => (A, T))
      (implicit T: Steppable.Aux[T, F], BF: Functor[F])
        : T =
      typeClassInstance.topDownCata[A](self, a)(f)
    def topDownCataM[M[_]: Monad, A]
      (a: A)
      (f: (A, T) => M[(A, T)])
      (implicit T: Steppable.Aux[T, F], BT: Traverse[F])
        : M[T] =
      typeClassInstance.topDownCataM[M, A](self, a)(f)
    object transPrepro {
      def apply[U] = new PartiallyApplied[U]
      final class PartiallyApplied[U] {
        def apply[G[_]: Functor]
          (e: F ~> F, f: Transform[U, F, G])
          (implicit
            T: Steppable.Aux[T, F],
            U: Steppable.Aux[U, G],
            BF: Functor[F])
            : U =
          typeClassInstance.transPrepro(self)(e, f)
      }
    }
    def transCataT(f: T => T)(implicit T: Steppable.Aux[T, F], BF: Functor[F])
        : T =
      typeClassInstance.transCataT(self)(f)

    def transParaT
      (f: ((T, T)) => T)
      (implicit T: Steppable.Aux[T, F], BF: Functor[F])
        : T =
      typeClassInstance.transParaT(self)(f)

    def transAnaT(f: T => T)(implicit T: Steppable.Aux[T, F], BF: Functor[F])
        : T =
      typeClassInstance.transAnaT(self)(f)

    def transApoT
      (f: T => Either[T, T])
      (implicit T: Steppable.Aux[T, F], BF: Functor[F])
        : T =
      typeClassInstance.transApoT(self)(f)

    def transCataTM[M[_]: Monad]
      (f: T => M[T])
      (implicit T: Steppable.Aux[T, F], BF: Traverse[F])
        : M[T] =
      typeClassInstance.transCataTM(self)(f)

    def transAnaTM[M[_]: Monad]
      (f: T => M[T])
      (implicit T: Steppable.Aux[T, F], BF: Traverse[F])
        : M[T] =
      typeClassInstance.transAnaTM(self)(f)
  }

  trait ToBirecursiveOps {
    implicit def toBirecursiveOps[T, F[_]](target: T)(implicit tc: Aux[T, F]): Ops[T, F] =
      new Ops[T, F] {
        val self = target
        val typeClassInstance = tc
      }
  }

  object nonInheritedOps extends ToBirecursiveOps

  trait AllOps[T, F[_]] extends Ops[T, F] with Recursive.Ops[T, F] {
    def typeClassInstance: Aux[T, F]
  }

  object ops {
    implicit def toAllBirecursiveOps[T, F[_]](target: T)(implicit tc: Aux[T, F]): AllOps[T, F] =
      new AllOps[T, F] {
        val self = target
        val typeClassInstance = tc
      }
  }
}
