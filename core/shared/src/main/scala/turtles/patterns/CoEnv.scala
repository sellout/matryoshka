/*
 * Copyright 2014–2017 SlamData Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package turtles.patterns

import slamdata.Predef.{Eq => _, _}
import turtles._
import turtles.derived._

import cats._
import cats.free._
import cats.functor._
import cats.implicits._

/** The pattern functor for Free. */
final case class CoEnv[E, F[_], A](run: Either[E, F[A]])

object CoEnv extends CoEnvInstances {
  def coEnv[E, F[_], A](v: Either[E, F[A]]): CoEnv[E, F, A] = CoEnv(v)

  def hmap[F[_], G[_], A](f: F ~> G) =
    λ[CoEnv[A, F, ?] ~> CoEnv[A, G, ?]](fa => CoEnv(fa.run.map(f(_))))

  def htraverse[G[_]: Applicative, F[_], H[_], A](f: F ~> (G ∘ H)#λ) =
    λ[CoEnv[A, F, ?] ~> (G ∘ CoEnv[A, H, ?])#λ](
      _.run.traverse(f(_)).map(CoEnv(_)))

  def freeIso[E, F[_]: Functor] = AlgebraIso[CoEnv[E, F, ?], Free[F, E]](
    coe => coe.run.fold(_.pure[Free[F, ?]], Free.roll))(
    fr => CoEnv(fr.fold(_.asLeft, _.asRight)))
}

sealed abstract class CoEnvInstances extends CoEnvInstances0 {
  implicit def equal[E: Eq, F[_]](implicit F: Delay[Eq, F]):
      Delay[Eq, CoEnv[E, F, ?]] =
    new Delay[Eq, CoEnv[E, F, ?]] {
      def apply[α](arb: Eq[α]) = {
        Eq.instance((a, b) => (a.run, b.run) match {
          case (Left(e1),  Left(e2))  => e1 === e2
          case (Right(f1), Right(f2)) => F(arb).eqv(f1, f2)
          case (_,         _)         => false
        })
      }
    }

  implicit def show[E: Show, F[_]](implicit F: Delay[Show, F]): Delay[Show, CoEnv[E, F, ?]] =
    new Delay[Show, CoEnv[E, F, ?]] {
      def apply[A](sh: Show[A]) =
        Show.show(
          _.run.fold(
            e  => "Left(" |+| e.show,
            fa => "Right(" |+| F(sh).show(fa)) |+| ")")
    }

  // TODO: Need to have lower-prio instances of Bifoldable, with
  //       corresponding constraint on F.
  implicit def bitraverse[F[_]: Traverse]: Bitraverse[CoEnv[?, F, ?]] =
    new Bitraverse[CoEnv[?, F, ?]] {
      def bitraverse[G[_]: Applicative, A, B, C, D](
        fab: CoEnv[A, F, B])(
        f: A ⇒ G[C], g: B ⇒ G[D]) =
        fab.run.bitraverse(f, _.traverse(g)).map(CoEnv(_))

      def bifoldLeft[A, B, C](
        fab: CoEnv[A, F, B], c: C)(
        f: (C, A) => C, g: (C, B) => C) =
        fab.run.bifoldLeft(c)(f, (c, b) => b.foldLeft(c)(g))

      def bifoldRight[A, B, C](
        fab: CoEnv[A, F, B],c: Eval[C])(
        f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]) =
        fab.run.bifoldRight(c)(f, _.foldRight(_)(g))
    }

  implicit def traverse[F[_]: Traverse, E]: Traverse[CoEnv[E, F, ?]] =
    bitraverseTraverse[CoEnv[?, F, ?], E]

  // TODO: write a test to ensure the two monad instances are identical
  // implicit def monadCo[F[_]: Applicative: Comonad, A]: Monad[CoEnv[A, F, ?]] =
  //   new Monad[CoEnv[A, F, ?]] {
  //     def bind[B, C](fa: CoEnv[A, F, B])(f: (B) ⇒ CoEnv[A, F, C]) =
  //       CoEnv(fa.run >>= (fb => f(fb.copoint).run))
  //     def point[B](x: => B) = CoEnv(x.point[F].right)
  //   }
}

sealed abstract class CoEnvInstances0 {
  implicit def bifunctor[F[_]: Functor]: Bifunctor[CoEnv[?, F, ?]] =
    new Bifunctor[CoEnv[?, F, ?]] {
      def bimap[A, B, C, D](fab: CoEnv[A, F, B])(f: A ⇒ C, g: B ⇒ D) =
        CoEnv(fab.run.bimap(f, _.map(g)))
    }

  implicit def functor[F[_]: Functor, E]: Functor[CoEnv[E, F, ?]] =
    bifunctorFunctor[CoEnv[?, F, ?], E]

  implicit def bifoldable[F[_]: Foldable]: Bifoldable[CoEnv[?, F, ?]] =
    new Bifoldable[CoEnv[?, F, ?]] {
      def bifoldLeft[A, B, C](
        fa: CoEnv[A, F, B], z: C)(
        f: (C, A) ⇒ C, g: (C, B) ⇒ C) =
        fa.run.fold(f(z, _), _.foldLeft(z)(g))

      def bifoldRight[A, B, C](
        fa: CoEnv[A, F, B], z: Eval[C])(
        f: (A, Eval[C]) ⇒ Eval[C], g: (B, Eval[C]) ⇒ Eval[C]) =
        fa.run.fold(f(_, z), _.foldRight(z)(g))
    }

  implicit def foldable[F[_]: Foldable, E]: Foldable[CoEnv[E, F, ?]] =
    bifoldableFoldable[CoEnv[?, F, ?], E]

  // implicit def monad[F[_]: Monad: Traverse, A]: Monad[CoEnv[A, F, ?]] =
  //   new Monad[CoEnv[A, F, ?]] {
  //     def bind[B, C](fa: CoEnv[A, F, B])(f: (B) ⇒ CoEnv[A, F, C]) =
  //       CoEnv(fa.run >>= (_.traverse[CoEnv[A, F, ?], C](f).run.map(_.flatten)))
  //     def point[B](x: => B) = CoEnv(x.point[F].right)
  //   }
}
