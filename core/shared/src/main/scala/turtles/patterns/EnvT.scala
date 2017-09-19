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

import turtles._
import turtles.derived._

import cats._
import cats.free._
import cats.implicits._

/** This is the transformer for the (,) comonad.
  */
final case class EnvT[E, W[_], A](run: (E, W[A])) { self =>
  import EnvT._

  def runEnvT: (E, W[A]) = run

  def ask: E = run._1

  def lower: W[A] = run._2

  def map[B](f: A => B)(implicit W: Functor[W]): EnvT[E, W, B] =
    envT((run._1, run._2.map(f)))
}

object EnvT extends EnvTInstances with EnvTFunctions {
  def hmap[F[_], G[_], E, A](f: F ~> G) =
    λ[EnvT[E, F, ?] ~> EnvT[E, G, ?]](env => EnvT((env.ask, f(env.lower))))

  def htraverse[G[_]: Applicative, F[_], H[_], A](f: F ~> (G ∘ H)#λ) =
    λ[EnvT[A, F, ?] ~> (G ∘ EnvT[A, H, ?])#λ](_.run.traverse(f(_)).map(EnvT(_)))

  def lower[F[_], E] = λ[EnvT[E, F, ?] ~> F](_.lower)
}

sealed abstract class EnvTInstances1 {
  implicit def functor[E, W[_]](implicit W0: Functor[W]):
      Functor[EnvT[E, W, ?]] =
    new EnvTFunctor[E, W] {
      implicit def W: Functor[W] = W0
    }
}

sealed abstract class EnvTInstances0 extends EnvTInstances1 {
  implicit def cobind[E, W[_]](implicit W0: CoflatMap[W]):
      CoflatMap[EnvT[E, W, ?]] =
    new EnvTCoflatMap[E, W] {
      implicit def W: CoflatMap[W] = W0
    }
}

sealed abstract class EnvTInstances extends EnvTInstances0 {
  implicit def comonad[E, W[_]](implicit W0: Comonad[W]): Comonad[EnvT[E, W, ?]] =
    new EnvTComonad[E, W] { implicit def W: Comonad[W] = W0 }

  implicit def equal[E: Eq, W[_]](implicit W: Delay[Eq, W]): Delay[Eq, EnvT[E, W, ?]] =
    new Delay[Eq, EnvT[E, W, ?]] {
      def apply[A](eq: Eq[A]) =
        Eq.instance((a, b) => a.ask === b.ask && W(eq).eqv(a.lower, b.lower))
    }

  implicit def show[E: Show, F[_]](implicit F: Delay[Show, F]): Delay[Show, EnvT[E, F, ?]] =
    new Delay[Show, EnvT[E, F, ?]] {
      def apply[A](sh: Show[A]) =
        Show.show(envt =>
          "EnvT(" |+| envt.ask.show |+| ", " |+| F(sh).show(envt.lower) |+| ")")
    }

  implicit def bitraverse[F[_]](implicit F0: Traverse[F]): Bitraverse[EnvT[?, F, ?]] =
    new EnvTBitraverse[F] { implicit def F: Traverse[F] = F0 }

  implicit def traverse[E, F[_]: Traverse]: Traverse[EnvT[E, F, ?]] =
    bitraverseTraverse[EnvT[?, F, ?], E]
}

trait EnvTFunctions {
  def envT[E, W[_], A](v: (E, W[A])): EnvT[E, W, A] = EnvT(v)

  def cofreeIso[E, W[_]] = AlgebraIso[EnvT[E, W, ?], Cofree[W, E]](
    et => Cofree(et.ask, Later(et.lower)))(
    cof => EnvT((cof.head, cof.tail.value)))
}

//
// Type class implementation traits
//

private trait EnvTFunctor[E, W[_]] extends Functor[EnvT[E, W, ?]] {
  implicit def W: Functor[W]

  override final def map[A, B](fa: EnvT[E, W, A])(f: A => B) = fa map f
}

private trait EnvTBitraverse[F[_]]
    extends Bitraverse[EnvT[?, F, ?]]
    // with EnvTBiFunctor[F]
    // with EnvTBifoldable[F]
{
  implicit def F: Traverse[F]

  override final def bitraverse[G[_]: Applicative, A, B, C, D]
    (fab: EnvT[A, F, B])
    (f: A ⇒ G[C], g: B ⇒ G[D]) =
    fab.run.bitraverse(f, _.traverse(g)).map(EnvT(_))

  override final def bifoldLeft[A, B, C](
    fab: EnvT[A, F, B], c: C)(
    f: (C, A) => C, g: (C, B) => C) =
    fab.run.bifoldLeft(c)(f, (c, b) => b.foldLeft(c)(g))

  override final def bifoldRight[A, B, C](
    fab: EnvT[A, F, B], c: Eval[C])(
    f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]) =
    fab.run.bifoldRight(c)(f, _.foldRight(_)(g))
}

private trait EnvTCoflatMap[E, W[_]] extends CoflatMap[EnvT[E, W, ?]] with EnvTFunctor[E, W] {
  implicit def W: CoflatMap[W]

  override final def coflatten[A](fa: EnvT[E, W, A]): EnvT[E, W, EnvT[E, W, A]] =
    EnvT((fa.ask, fa.lower.coflatMap(x => EnvT((fa.ask, x)))))

  override final def coflatMap[A, B](fa: EnvT[E, W, A])(f: EnvT[E, W, A] => B): EnvT[E, W, B] =
    coflatten(fa).map(f)
}

private trait EnvTComonad[E, W[_]] extends Comonad[EnvT[E, W, ?]] with EnvTCoflatMap[E, W] {
  implicit def W: Comonad[W]

  def extract[A](p: EnvT[E, W, A]): A = p.lower.extract
}
