/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles

import slamdata.Predef.{Eq => _, _}
import turtles.derived._
import turtles.implicits._
import turtles.patterns._

import cats._
import cats.data._
import cats.free._
import cats.implicits._
import newts.syntax.all._

/** Folds for recursive data types. */
trait Recursive[T] extends Based[T] { self =>
  // TODO: This works around a bug in Simulacrum (#55). Delete once that is fixed.
  type BaseT[A] = Base[A]

  implicit val rec: Recursive.Aux[T, Base] = self

  /** The fundamental operation on all finite data types. It can be seen as a
    * generalization of `Foldable.foldRight` (or rather, `Foldable.foldRight`
    * is a specialization of this to [[scala.List]]).
    */
  def cata[A](t: T)(φ: Algebra[Base, A]): A

  /** A Kleisli catamorphism. */
  def cataM[M[_]: Monad, A]
    (t: T)
    (φ: AlgebraM[M, Base, A])
    (implicit BT: Traverse[Base])
      : M[A] =
    cata[M[A]](t)(_.sequence >>= φ)

  /** A catamorphism generalized with a comonad inside the functor. */
  def gcata[W[_]: Comonad, A]
    (t: T)
    (k: DistributiveLaw[Base, W], φ: GAlgebra[W, Base, A])
    (implicit BF: Functor[Base])
      : A =
    cata[W[A]](t)(fwa => k(fwa.map(_.coflatten)).map(φ)).extract

  def gcataM[W[_]: Comonad: Traverse, M[_]: Monad, A]
    (t: T)
    (w: DistributiveLaw[Base, W], φ: GAlgebraM[W, M, Base, A])
    (implicit BT: Traverse[Base])
      : M[A] =
    cataM[M, W[A]](t)(fwa => w(fwa.map(_.coflatten)).traverse(φ)).map(_.extract)

  /** A catamorphism generalized with a comonad outside the functor. */
  def elgotCata[W[_]: Comonad, A](
    t: T)(
    k: DistributiveLaw[Base, W], φ: ElgotAlgebra[W, Base, A])
    (implicit BF: Functor[Base])
      : A =
    φ(cata[W[Base[A]]](t)(fwfa => k(fwfa.map(_.coflatMap(φ)))))

  def elgotCataM[W[_]: Comonad : Traverse, M[_]: Monad, A]
    (t: T)
    (k: DistributiveLaw[Base, (M ∘ W)#λ], φ: ElgotAlgebraM[W, M, Base, A])
    (implicit BT: Traverse[Base])
      : M[A] =
    cataM[M, W[Base[A]]](t)(fwfa => k(fwfa.map(_.coflatten.traverse(φ)))) >>= φ

  def para[A]
    (t: T)
    (φ: GAlgebra[(T, ?), Base, A])
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : A =
    gcata[(T, ?), A](t)(distPara, φ)

  def elgotPara[A]
    (t: T)
    (φ: ElgotAlgebra[(T, ?), Base, A])
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : A =
    elgotCata[(T, ?), A](t)(distPara, φ)

  def gpara[W[_]: Comonad, A]
    (t: T)
    (e: DistributiveLaw[Base, W], φ: GAlgebra[EnvT[T, W, ?], Base, A])
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : A =
    gzygo[W, A, T](t)(T.embed, e, φ)

  def paraM[M[_]: Monad, A]
    (t: T)
    (φ: GAlgebraM[(T, ?), M, Base, A])
    (implicit T: Steppable.Aux[T, Base], BT: Traverse[Base])
      : M[A] =
    para[M[A]](t)(_.map(_.sequence).sequence >>= φ)

  def zygo[A, B]
    (t: T)
    (f: Algebra[Base, B], φ: GAlgebra[(B, ?), Base, A])
    (implicit BF: Functor[Base])
      : A =
    gcata[(B, ?), A](t)(distZygo(f), φ)

  def zygoM[A, B, M[_]: Monad]
    (t: T)
    (φʹ: AlgebraM[M, Base, B], φ: GAlgebraM[(B, ?), M, Base, A])
    (implicit BT: Traverse[Base])
      : M[A] =
    gcataM[(M[B], ?), M, A](
      t)(
      distZygo(_.sequence >>= φʹ),
        _.traverse(_.swap.sequence.map(_.swap)) >>= φ)

  def elgotZygo[A, B]
    (t: T)
    (φʹ: Algebra[Base, B], φ: ElgotAlgebra[(B, ?), Base, A])
    (implicit BF: Functor[Base])
      : A =
    elgotCata[(B, ?), A](t)(distZygo(φʹ), φ)

  def elgotZygoM[A, B, M[_]: Monad]
    (t: T)
    (φʹ: AlgebraM[M, Base, B], φ: ElgotAlgebraM[(B, ?), M, Base, A])
    (implicit BT: Traverse[Base])
      : M[A] =
    elgotCataM[(B, ?), M, A](t)(distZygoM(φʹ, distApplicative[Base, M]), φ)

  def gzygo[W[_]: Comonad, A, B]
    (t: T)
    (φʹ: Algebra[Base, B],
      w: DistributiveLaw[Base, W],
      φ: GAlgebra[EnvT[B, W, ?], Base, A])
    (implicit BF: Functor[Base])
      : A =
    gcata[EnvT[B, W, ?], A](t)(distZygoT(φʹ, w), φ)

  def gElgotZygo[W[_]: Comonad, A, B]
    (t: T)
    (φʹ: Algebra[Base, B],
      w: DistributiveLaw[Base, W],
      φ: ElgotAlgebra[EnvT[B, W, ?], Base, A])
    (implicit BF: Functor[Base])
      : A =
    elgotCata[EnvT[B, W, ?], A](t)(distZygoT(φʹ, w), φ)

  /** Mutually-recursive fold. */
  def mutu[A, B]
    (t: T)
    (φʹ: GAlgebra[(A, ?), Base, B], φ: GAlgebra[(B, ?), Base, A])
    (implicit BF: Functor[Base])
      : A =
    cata(t)(φʹ <<< BF.lift((_: (B, A)).swap) &&& φ)._2

  def histo[A]
    (t: T)
    (φ: GAlgebra[Cofree[Base, ?], Base, A])
    (implicit BF: Functor[Base])
      : A =
    gcata[Cofree[Base, ?], A](t)(distHisto, φ)

  def elgotHisto[A]
    (t: T)
    (φ: ElgotAlgebra[Cofree[Base, ?], Base, A])
    (implicit BF: Functor[Base])
      : A =
    elgotCata[Cofree[Base, ?], A](t)(distHisto, φ)

  def ghisto[H[_]: Functor, A](
    t: T)(
    g: DistributiveLaw[Base, H], φ: GAlgebra[Cofree[H, ?], Base, A])
    (implicit BF: Functor[Base]):
      A =
    gcata[Cofree[H, ?], A](t)(distGHisto(g), φ)

  def prepro[A]
    (t: T)
    (e: Base ~> Base, φ: Algebra[Base, A])
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : A =
    gprepro[Id, A](t)(distCata, e, φ)

  def gprepro[W[_]: Comonad, A](
    t: T)(
    k: DistributiveLaw[Base, W], e: Base ~> Base, φ: GAlgebra[W, Base, A])(
    implicit T: Steppable.Aux[T, Base], BF: Functor[Base]):
      A =
    ghylo[W, Id, Base, T, A](
      t)(
      k, distAna, φ, T.project(_).map(cata[T](_)(e(_).embed)))

  def gcataZygo[W[_]: Comonad, A, B]
    (t: T)
    (k: DistributiveLaw[Base, W],
      φʹ: GAlgebra[W, Base, B],
      φ: GAlgebra[(B, ?), Base, A])
    (implicit BF: Functor[Base], BU: Alternative[Base])
      : A =
    gcata[(W[B], ?), A](
      t)(
      distZygo(fwa => k(fwa.map(_.coflatten)).map(φʹ)),
        fwa => φ(fwa.map(_.leftMap(_.extract))))

  def paraZygo[A, B]
    (t: T)
    (φʹ: GAlgebra[(T, ?), Base, B], φ: GAlgebra[(B, ?), Base, A])
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base])
      : A = {
    @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
    def h(t: T): (B, A) = {
      val tmp = T.project(t).map { x =>
        val (b, a) = h(x)
        ((x, b), (b, a))
      }

      (tmp.map(_._1), tmp.map(_._2)).bimap(φʹ, φ)
    }

    h(t)._2
  }

  // FIXME: This version adds an extra implicit.
  // def paraZygo[A, B]
  //   (t: T)
  //   (φʹ: GAlgebra[(T, ?), Base, B], φ: GAlgebra[(B, ?), Base, A])
  //   (implicit
  //     T: Steppable.Aux[T, Base],
  //     BF: Functor[Base],
  //     BU: Alternative[Base]) =
  //   gcataZygo[(T, ?), A, B](t)(distPara, φʹ, φ)

  /** Combines two functors that may fail to merge, also providing access to the
    * inputs at each level. This is akin to an Elgot, not generalized, fold.
    */
  def paraMerga[A]
    (t: T, that: T)
    (φ: (T, T, Option[Base[A]]) => A)
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base], BM: Merge[Base])
      : A =
    hylo[λ[α => OptionT[(T, T, ?), Base[α]]], (T, T), A](
      (t, that))(
      fa => φ.tupled(fa.value),
        { case (a, b) => OptionT((a, b, T.project(a).merge(T.project(b)))) })(
      Functor[OptionT[(T, T, ?), ?]] compose BF)

  def isLeaf
    (t: T)
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base], B: Foldable[Base])
      : Boolean =
    T.project(t).foldRight(Now(true))((_, _) => Now(false)).value

  def children[U]
    (t: T)
    (implicit
      T: Steppable.Aux[T, Base],
      U: Steppable.Aux[U, ListF[T, ?]],
      BF: Functor[Base],
      B: Foldable[Base])
      : U =
    T.project(t).foldRight[U](Now(NilF[T, U]().embed))((a, b) => b.map(ConsF(a, _).embed)).value

  /** Attribute a tree via an algebra starting from the root. */
  def attributeTopDown[U, A]
    (t: T, z: A)
    (f: (A, Base[T]) => A)
    (implicit
      T: Steppable.Aux[T, Base],
      U: Corecursive.Aux[U, EnvT[A, Base, ?]],
      BF: Functor[Base])
      : U =
    U.ana((z, t)){ case (a, t) =>
      val ft = T.project(t)
      val aʹ = f(a, ft)
      EnvT((aʹ, ft.map((aʹ, _))))
    }

  /** Kleisli variant of attributeTopDown */
  def attributeTopDownM[M[_]: Monad, U, A]
    (t: T, z: A)
    (f: (A, Base[T]) => M[A])
    (implicit
      T: Steppable.Aux[T, Base],
      US: Steppable.Aux[U, EnvT[A, Base, ?]],
      UC: Corecursive.Aux[U, EnvT[A, Base, ?]],
      BT: Traverse[Base])
      : M[U] =
    UC.anaM((z, t)){ case (a, t) =>
      val ft = T.project(t)
      f(a, ft).map(aʹ => EnvT((aʹ, ft.map((aʹ, _)))))
    }

  // Foldable
  def all
    (t: T)
    (p: T ⇒ Boolean)
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base], B: Foldable[Base])
      : Boolean =
    foldMap(t)(p(_).asAll).unwrap

  def any
    (t: T)
    (p: T ⇒ Boolean)
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base], B: Foldable[Base])
      : Boolean =
    foldMap(t)(p(_).asAny).unwrap

  def collect[U: Monoid, B]
    (t: T)
    (pf: PartialFunction[T, B])
    (implicit
      T: Steppable.Aux[T, Base],
      U: Steppable.Aux[U, ListF[B, ?]],
      BF: Functor[Base],
      B: Foldable[Base])
      : U =
    foldMap(t)(pf.lift(_).foldRight[U](Now(NilF[B, U]().embed))((a, b) => b.map(ConsF(a, _).embed)).value)

  def contains
    (t: T, c: T)
    (implicit
      TS: Steppable.Aux[T, Base],
      TE: Eq[T],
      BF: Functor[Base],
      B: Foldable[Base])
      : Boolean =
    any(t)(_ === c)

  def foldMap[Z: Monoid]
    (t: T)
    (f: T => Z)
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base], B: Foldable[Base])
      : Z =
    foldMapM[Eval, Z](t)(f(_).pure[Eval]).value

  def foldMapM[M[_]: Monad, Z: Monoid]
    (t: T)
    (f: T => M[Z])
    (implicit T: Steppable.Aux[T, Base], BF: Functor[Base], B: Foldable[Base])
      : M[Z] = {
    @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
    def loop(z0: Z, term: T): M[Z] = {
      for {
        z1 <- f(term)
        z2 <- T.project(term).foldLeftM(z0 |+| z1)(loop(_, _))
      } yield z2
    }

    loop(Monoid[Z].empty, t)
  }

  /** Converts from this type to any [Steppable] type. I.e., you can
    * “expand” and least fixed point to another fixed point.
    *
    * This is normally unnecessary, unless some function you are passing
    * it to is insufficiently generalized.
    */
  def convertTo[R](t: T)(implicit R: Steppable.Aux[R, Base]): R =
    cata[R](t)(R.embed)

  def transCata[U, G[_]: Functor]
    (t: T)
    (f: Base[U] => G[U])
    (implicit U: Steppable.Aux[U, G], BF: Functor[Base])
      : U =
    cata(t)(f >>> (U.embed(_)))

  def transPrepro[U, G[_]: Functor]
    (t: T)
    (e: Base ~> Base, f: Transform[U, Base, G])
    (implicit
      T: Steppable.Aux[T, Base],
      U: Steppable.Aux[U, G],
      BF: Functor[Base])
      : U =
    prepro(t)(e, f >>> (U.embed(_)))

  def transPostpro[U, G[_]: Functor]
    (t: T)
    (e: G ~> G, f: Transform[T, Base, G])
    (implicit
      T: Steppable.Aux[T, Base],
      US: Steppable.Aux[U, G],
      UC: Corecursive.Aux[U, G],
      BF: Functor[Base])
      : U =
    UC.postpro(t)(e, f <<< T.project)

  def transGcata[W[_]: Comonad, U, G[_]: Functor]
    (t: T)
    (k: DistributiveLaw[Base, W], f: AlgebraicGTransform[W, U, Base, G])
    (implicit U: Steppable.Aux[U, G], BF: Functor[Base])
     : U =
    gcata(t)(k, f >>> (U.embed(_)))

  def transPara[U, G[_]: Functor]
    (t: T)
    (f: AlgebraicGTransform[(T, ?), U, Base, G])
    (implicit
      T: Steppable.Aux[T, Base],
      U: Steppable.Aux[U, G],
      BF: Functor[Base])
      : U =
    transGcata(t)(distPara, f)

  def transCataM[M[_]: Monad, U, G[_]: Functor]
    (t: T)
    (f: TransformM[M, U, Base, G])
    (implicit U: Steppable.Aux[U, G], BT: Traverse[Base])
      : M[U] =
    cataM(t)(f(_).map(U.embed(_)))

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

  def transCataTM[M[_]: Monad]
    (t: T)
    (f: T => M[T])
    (implicit T: Steppable.Aux[T, Base], BF: Traverse[Base])
      : M[T] =
    cataM(t)(f <<< T.embed)
}

object Recursive {
  /** Create a [[Recursive]] instance from the mappings to/from the
    * fixed-point.
    */
  def withNativeRecursion[T, F[_]]
    (implicit T: Steppable.Aux[T, F], F: Functor[F])
      : Recursive.Aux[T, F] =
    new Recursive[T] {
      type Base[A] = F[A]
      def cata[A](t: T)(f: Algebra[Base, A]) = hylo(t)(f, T.project)
    }

  def show[T, F[_]: Functor](implicit T: Recursive.Aux[T, F], F: Delay[Show, F])
      : Show[T] =
    Show.show(T.cata(_)(F(Show[String]).show))

  // NB: The rest of this is what would be generated by simulacrum, except this
  //     type class is too complicated to take advantage of that.

  type Aux[T, F[_]] = Recursive[T] { type Base[A] = F[A] }

  def apply[T](implicit instance: Recursive[T]): Aux[T, instance.Base] =
    instance

  trait Ops[T, F[_]] {
    def typeClassInstance: Aux[T, F]
    def self: T

    def cata[A](f: Algebra[F, A])(implicit BF: Functor[F]): A =
      typeClassInstance.cata[A](self)(f)
    def cataM[M[_]: Monad, A](f: AlgebraM[M, F, A])(implicit BT: Traverse[F])
        : M[A] =
      typeClassInstance.cataM[M, A](self)(f)
    def gcata[W[_]: Comonad, A]
      (k: DistributiveLaw[F, W], g: GAlgebra[W, F, A])
      (implicit BF: Functor[F])
        : A =
      typeClassInstance.gcata[W, A](self)(k, g)
    def gcataM[W[_]: Comonad: Traverse, M[_]: Monad, A]
      (w: DistributiveLaw[F, W], g: GAlgebraM[W, M, F, A])
      (implicit BT: Traverse[F])
        : M[A] =
      typeClassInstance.gcataM[W, M, A](self)(w, g)
    def elgotCata[W[_]: Comonad, A]
      (k: DistributiveLaw[F, W], g: ElgotAlgebra[W, F, A])
      (implicit BF: Functor[F])
        : A =
      typeClassInstance.elgotCata[W, A](self)(k, g)
    def elgotCataM[W[_]: Comonad : Traverse, M[_]: Monad, A]
      (k: DistributiveLaw[F, (M ∘ W)#λ], g: ElgotAlgebraM[W, M, F, A])
      (implicit BT: Traverse[F])
        : M[A] =
      typeClassInstance.elgotCataM[W, M, A](self)(k, g)
    def para[A]
      (f: GAlgebra[(T, ?), F, A])
      (implicit T: Steppable.Aux[T, F], BF: Functor[F])
        : A =
      typeClassInstance.para[A](self)(f)
    def elgotPara[A]
      (f: ElgotAlgebra[(T, ?), F, A])
      (implicit T: Steppable.Aux[T, F], BF: Functor[F])
        : A =
      typeClassInstance.elgotPara[A](self)(f)
    def gpara[W[_]: Comonad, A]
      (e: DistributiveLaw[F, W], f: GAlgebra[EnvT[T, W, ?], F, A])
      (implicit T: Steppable.Aux[T, F], BF: Functor[F])
        : A =
      typeClassInstance.gpara[W, A](self)(e, f)
    def paraM[M[_]: Monad, A]
      (f: GAlgebraM[(T, ?), M, F, A])
      (implicit T: Steppable.Aux[T, F], BT: Traverse[F])
        : M[A] =
      typeClassInstance.paraM[M, A](self)(f)
    def zygo[A, B]
      (f: Algebra[F, B], g: GAlgebra[(B, ?), F, A])
      (implicit BF: Functor[F])
        : A =
      typeClassInstance.zygo[A, B](self)(f, g)
    def zygoM[A, B, M[_]: Monad]
      (f: AlgebraM[M, F, B], g: GAlgebraM[(B, ?), M, F, A])
      (implicit BT: Traverse[F])
        : M[A] =
      typeClassInstance.zygoM[A, B, M](self)(f, g)
    def elgotZygo[A, B]
      (f: Algebra[F, B], g: ElgotAlgebra[(B, ?), F, A])
      (implicit BF: Functor[F])
        : A =
      typeClassInstance.elgotZygo[A, B](self)(f, g)
    def elgotZygoM[A, B, M[_]: Monad]
      (f: AlgebraM[M, F, B], g: ElgotAlgebraM[(B, ?), M, F, A])
      (implicit BT: Traverse[F])
        : M[A] =
      typeClassInstance.elgotZygoM[A, B, M](self)(f, g)
    def gzygo[W[_]: Comonad, A, B]
      (f: Algebra[F, B],
        w: DistributiveLaw[F, W],
        g: GAlgebra[EnvT[B, W, ?], F, A])
      (implicit BF: Functor[F])
        : A =
      typeClassInstance.gzygo[W, A, B](self)(f, w, g)
    def gElgotZygo[W[_]: Comonad, A, B]
      (f: Algebra[F, B],
        w: DistributiveLaw[F, W],
        g: ElgotAlgebra[EnvT[B, W, ?], F, A])
      (implicit BF: Functor[F])
        : A =
      typeClassInstance.gElgotZygo [W, A, B](self)(f, w, g)
    def mutu[A, B]
      (f: GAlgebra[(A, ?), F, B], g: GAlgebra[(B, ?), F, A])
      (implicit T: Steppable.Aux[T, F], BF: Functor[F])
        : A =
      typeClassInstance.mutu[A, B](self)(f, g)
    def histo[A]
      (f: GAlgebra[Cofree[F, ?], F, A])
      (implicit BF: Functor[F])
        : A =
      typeClassInstance.histo(self)(f)
    def elgotHisto[A]
      (f: ElgotAlgebra[Cofree[F, ?], F, A])
      (implicit BF: Functor[F])
        : A =
      typeClassInstance.elgotHisto(self)(f)
    def ghisto[H[_]: Functor, A]
      (g: DistributiveLaw[F, H], f: GAlgebra[Cofree[H, ?], F, A])
      (implicit BF: Functor[F])
        : A =
      typeClassInstance.ghisto(self)(g, f)
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
    def gcataZygo[W[_]: Comonad, A, B]
      (w: DistributiveLaw[F, W], f: GAlgebra[W, F, B], g: GAlgebra[(B, ?), F, A])
      (implicit BF: Functor[F], BU: Alternative[F])
        : A =
      typeClassInstance.gcataZygo[W, A, B](self)(w, f, g)
    def paraZygo[A, B]
      (f: GAlgebra[(T, ?), F, B], g: GAlgebra[(B, ?), F, A])
      (implicit T: Steppable.Aux[T, F], BF: Functor[F], BU: Alternative[F])
        : A =
      typeClassInstance.paraZygo[A, B](self)(f, g)
    def paraMerga[A]
      (that: T)
      (f: (T, T, Option[F[A]]) => A)
      (implicit T: Steppable.Aux[T, F], BF: Functor[F], BM: Merge[F])
        : A =
      typeClassInstance.paraMerga[A](self, that)(f)
    def isLeaf(implicit T: Steppable.Aux[T, F], BT: Traverse[F]): Boolean =
      typeClassInstance.isLeaf(self)
    def children[U]
      (implicit
        T: Steppable.Aux[T, F],
        U: Steppable.Aux[U, ListF[T, ?]],
        BT: Traverse[F])
        : U =
      typeClassInstance.children(self)
    def attributeTopDown[U, A]
      (z: A)
      (f: (A, F[T]) => A)
      (implicit
        T: Steppable.Aux[T, F],
        U: Corecursive.Aux [U, EnvT[A, F, ?]],
        BF: Functor[F])
        : U =
      typeClassInstance.attributeTopDown[U, A](self, z)(f)
    def attributeTopDownM[M[_]: Monad, U, A]
      (z: A)
      (f: (A, F[T]) => M[A])
      (implicit
        T: Steppable.Aux[T, F],
        US: Steppable.Aux[U, EnvT[A, F, ?]],
        UC: Corecursive.Aux[U, EnvT[A, F, ?]],
        BT: Traverse[F])
        : M[U] =
      typeClassInstance.attributeTopDownM[M, U, A](self, z)(f)
    def all
      (p: T ⇒ Boolean)
      (implicit T: Steppable.Aux[T, F], BF: Functor[F], B: Foldable[F])
        : Boolean =
      typeClassInstance.all(self)(p)
    def any
      (p: T ⇒ Boolean)
      (implicit T: Steppable.Aux[T, F], BF: Functor[F], B: Foldable[F])
        : Boolean =
      typeClassInstance.any(self)(p)
    def collect[U: Monoid, B]
      (pf: PartialFunction[T, B])
      (implicit
        T: Steppable.Aux[T, F],
        US: Steppable.Aux[U, ListF[B, ?]],
        UB: Corecursive.Aux[U, ListF[B, ?]],
        BF: Functor[F],
        B: Foldable[F])
        : U =
      typeClassInstance.collect[U, B](self)(pf)
    def contains
      (c: T)
      (implicit
        TS: Steppable.Aux[T, F],
        TE: Eq[T],
        BF: Functor[F],
        B: Foldable[F])
        : Boolean =
      typeClassInstance.contains(self, c)
    def foldMap[Z: Monoid]
      (f: T => Z)
      (implicit T: Steppable.Aux[T, F], BF: Functor[F], B: Foldable[F])
        : Z =
      typeClassInstance.foldMap[Z](self)(f)
    def foldMapM[M[_]: Monad, Z: Monoid]
      (f: T => M[Z])
      (implicit T: Steppable.Aux[T, F], BF: Functor[F], B: Foldable[F])
        : M[Z] =
      typeClassInstance.foldMapM[M, Z](self)(f)
    def convertTo[R](implicit R: Steppable.Aux[R, F], BF: Functor[F])
        : R =
      typeClassInstance.convertTo[R](self)

    object transCata {
      def apply[U] = new PartiallyApplied[U]
      final class PartiallyApplied[U] {
        def apply[G[_]: Functor]
          (f: F[U] => G[U])
          (implicit U: Steppable.Aux[U, G], BF: Functor[F])
            : U =
          typeClassInstance.transCata(self)(f)
      }
    }

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

    object transPostpro {
      def apply[U] = new PartiallyApplied[U]
      final class PartiallyApplied[U] {
        def apply[G[_]: Functor]
          (e: G ~> G, f: Transform[T, F, G])
          (implicit
            T: Steppable.Aux[T, F],
            US: Steppable.Aux[U, G],
            UC: Corecursive.Aux[U, G],
            BF: Functor[F])
            : U =
          typeClassInstance.transPostpro(self)(e, f)
      }
    }

    object transPara {
      def apply[U] = new PartiallyApplied[U]
      final class PartiallyApplied[U] {
        def apply[G[_]: Functor]
          (f: AlgebraicGTransform[(T, ?), U, F, G])
          (implicit
            T: Steppable.Aux[T, F],
            U: Steppable.Aux[U, G],
            BF: Functor[F])
            : U =
          typeClassInstance.transPara(self)(f)
      }
    }

    def transCataM[M[_]: Monad, U, G[_]: Functor]
      (f: TransformM[M, U, F, G])
      (implicit U: Steppable.Aux[U, G], BT: Traverse[F])
        : M[U] =
      typeClassInstance.transCataM(self)(f)

    def transCataT(f: T => T)(implicit T: Steppable.Aux[T, F], BF: Functor[F])
        : T =
      typeClassInstance.transCataT(self)(f)

    def transParaT
      (f: ((T, T)) => T)
      (implicit T: Steppable.Aux[T, F], BF: Functor[F])
        : T =
      typeClassInstance.transParaT(self)(f)

    def transCataTM[M[_]: Monad]
      (f: T => M[T])
      (implicit T: Steppable.Aux[T, F], BF: Traverse[F])
        : M[T] =
      typeClassInstance.transCataTM(self)(f)
  }

  trait ToRecursiveOps {
    implicit def toRecursiveOps[T, F[_]](target: T)(implicit tc: Aux[T, F]): Ops[T, F] =
      new Ops[T, F] {
        val self = target
        val typeClassInstance = tc
      }
  }

  object nonInheritedOps extends ToRecursiveOps

  trait AllOps[T, F[_]] extends Ops[T, F] {
    def typeClassInstance: Aux[T, F]
  }

  object ops {
    implicit def toAllRecursiveOps[T, F[_]](target: T)(implicit tc: Aux[T, F]): AllOps[T, F] =
      new AllOps[T, F] {
        val self = target
        val typeClassInstance = tc
      }
  }
}
