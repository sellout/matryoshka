/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.implicits

import slamdata.Predef.{Eq => _, _}
import turtles._

import cats._
import cats.free._

sealed class IdOps[A](self: A) {
  def hylo[F[_]: Functor, B](f: Algebra[F, B], g: Coalgebra[F, A]): B =
    turtles.hylo(self)(f, g)

  def hyloM[M[_]: Monad, F[_]: Traverse, B](f: AlgebraM[M, F, B], g: CoalgebraM[M, F, A]):
      M[B] =
    turtles.hyloM(self)(f, g)

  object ghylo {
    def apply[W[_], N[_]] = new PartiallyApplied[W, N]

    final class PartiallyApplied[W[_], N[_]] {
      def apply[F[_]: Functor, B]
        (w: DistributiveLaw[F, W],
          n: DistributiveLaw[N, F],
          f: GAlgebra[W, F, B],
          g: GCoalgebra[N, F, A])
        (implicit W: Comonad[W], N: Monad[N]) =
        turtles.ghylo(self)(w, n, f, g)
    }
  }

  def ghyloM[W[_]: Comonad: Traverse, N[_]: Monad: Traverse, M[_]: Monad, F[_]: Traverse, B](
    w: DistributiveLaw[F, W],
    n: DistributiveLaw[N, F],
    f: GAlgebraM[W, M, F, B],
    g: GCoalgebraM[N, M, F, A]):
      M[B] =
    turtles.ghyloM(self)(w, n, f, g)

  def dyna[F[_]: Functor, B](φ: GAlgebra[Cofree[F, ?], F, B], ψ: Coalgebra[F, A]): B =
    turtles.dyna(self)(φ, ψ)

  def codyna[F[_]: Functor, B](φ: Algebra[F, B], ψ: GCoalgebra[Free[F, ?], F, A]): B =
    turtles.codyna(self)(φ, ψ)

  def codynaM[M[_]: Monad, F[_]: Traverse, B](φ: AlgebraM[M, F, B], ψ: GCoalgebraM[Free[F, ?], M, F, A]): M[B] =
    turtles.codynaM(self)(φ, ψ)

  def chrono[F[_]: Functor, B](
    g: GAlgebra[Cofree[F, ?], F, B], f: GCoalgebra[Free[F, ?], F, A]):
      B =
    turtles.chrono(self)(g, f)

  def elgot[F[_]: Functor, B](φ: Algebra[F, B], ψ: ElgotCoalgebra[Either[B, ?], F, A]): B =
    turtles.elgot(self)(φ, ψ)

  def coelgot[F[_]: Functor, B](φ: ElgotAlgebra[(A, ?), F, B], ψ: Coalgebra[F, A]): B =
    turtles.coelgot(self)(φ, ψ)

  object coelgotM {
    def apply[M[_]] = new PartiallyApplied[M]

    final class PartiallyApplied[M[_]] {
      def apply[F[_]: Traverse, B](φ: ElgotAlgebraM[(A, ?), M, F, B], ψ: CoalgebraM[M, F, A])(implicit M: Monad[M]):
          M[B] =
        turtles.coelgotM[M].apply[F, A, B](self)(φ, ψ)
    }
  }

  object ana {
    def apply[T] = new PartiallyApplied[T]

    final class PartiallyApplied[T] {
      def apply[F[_]: Functor]
        (f: Coalgebra[F, A])
        (implicit T: Corecursive.Aux[T, F])
          : T =
        T.ana(self)(f)
    }
  }

  object anaM {
    def apply[T] = new PartiallyApplied[T]

    final class PartiallyApplied[T] {
      def apply[M[_]: Monad, F[_]: Traverse]
        (f: CoalgebraM[M, F, A])
        (implicit TS: Steppable.Aux[T, F], TC: Corecursive.Aux[T, F])
          : M[T] =
        TC.anaM(self)(f)
    }
  }

  object gana {
    def apply[T] = new PartiallyApplied[T]

    final class PartiallyApplied[T] {
      def apply[N[_]: Monad, F[_]: Functor]
        (k: DistributiveLaw[N, F], f: GCoalgebra[N, F, A])
        (implicit T: Corecursive.Aux[T, F])
          : T =
        T.gana(self)(k, f)
    }
  }

  object ganaM {
    def apply[T] = new PartiallyApplied[T]

    final class PartiallyApplied[T] {
      def apply[N[_]: Monad: Traverse, M[_]: Monad, F[_]: Traverse]
        (k: DistributiveLaw[N, F], f: GCoalgebraM[N, M, F, A])
        (implicit TS: Steppable.Aux[T, F], TC: Corecursive.Aux[T, F])
          : M[T] =
        TC.ganaM(self)(k, f)
    }
  }

  object elgotAna {
    def apply[T] = new PartiallyApplied[T]

    final class PartiallyApplied[T] {
      def apply[N[_]: Monad, F[_]: Functor]
        (k: DistributiveLaw[N, F], f: ElgotCoalgebra[N, F, A])
        (implicit T: Corecursive.Aux[T, F])
          : T =
        T.elgotAna(self)(k, f)
    }
  }

  object apo {
    def apply[T] = new PartiallyApplied[T]

    final class PartiallyApplied[T] {
      def apply[F[_]: Functor]
        (f: GCoalgebra[Either[T, ?], F, A])
        (implicit TS: Steppable.Aux[T, F], TC: Corecursive.Aux[T, F])
          : T =
        TC.apo(self)(f)
    }
  }

  object apoM {
    def apply[T] = new PartiallyApplied[T]

    final class PartiallyApplied[T] {
      def apply[M[_]: Monad, F[_]: Traverse]
        (f: GCoalgebraM[Either[T, ?], M, F, A])
        (implicit TS: Steppable.Aux[T, F], TC: Corecursive.Aux[T, F])
          : M[T] =
        TC.apoM(self)(f)
    }
  }

  object gapo {
    def apply[T] = new PartiallyApplied[T]

    final class PartiallyApplied[T] {
      def apply[F[_]: Functor, B]
        (f: Coalgebra[F, B], g: GCoalgebra[Either[B, ?], F, A])
        (implicit TS: Steppable.Aux[T, F], TC: Corecursive.Aux[T, F])
          : T =
        TC.gapo(self)(f, g)
    }
  }

  object elgotApo {
    def apply[T] = new PartiallyApplied[T]

    final class PartiallyApplied[T] {
      def apply[F[_]: Functor]
        (f: ElgotCoalgebra[Either[T, ?], F, A])
        (implicit TS: Steppable.Aux[T, F], TC: Corecursive.Aux[T, F])
          : T =
        TC.elgotApo(self)(f)
    }
  }

  object postpro {
    def apply[T] = new PartiallyApplied[T]

    final class PartiallyApplied[T] {
      def apply[F[_]: Functor]
        (e: F ~> F, g: Coalgebra[F, A])
        (implicit TS: Steppable.Aux[T, F], TB: Birecursive.Aux[T, F])
          : T =
        TB.postpro(self)(e, g)
    }
  }

  object gpostpro {
    def apply[T] = new PartiallyApplied[T]

    final class PartiallyApplied[T] {
      def apply[N[_]: Monad, F[_]: Functor]
        (k: DistributiveLaw[N, F], e: F ~> F, g: GCoalgebra[N, F, A])
        (implicit TS: Steppable.Aux[T, F], TB: Birecursive.Aux[T, F])
          : T =
        TB.gpostpro(self)(k, e, g)
    }
  }

  object futu {
    def apply[T] = new PartiallyApplied[T]

    final class PartiallyApplied[T] {
      def apply[F[_]: Functor]
        (f: GCoalgebra[Free[F, ?], F, A])
        (implicit T: Corecursive.Aux[T, F])
          : T =
        T.futu(self)(f)
    }
  }

  def futuM[T, M[_]: Monad, F[_]: Traverse]
    (f: GCoalgebraM[Free[F, ?], M, F, A])
    (implicit TS: Steppable.Aux[T, F], TC: Corecursive.Aux[T, F])
      : M[T] =
    TC.futuM(self)(f)
}
