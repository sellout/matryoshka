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

package turtles

import cats._

/** This package contains derivable instances for various types that are
  * required by Turtles.
  */
package object derived extends DerivedInstances {
  implicit def tuple3Functor[A, B]: Functor[(A, B, ?)] =
    new Functor[(A, B, ?)] {
      def map[T, U](fa: (A, B, T))(f: T => U) =
        (fa._1, fa._2, f(fa._3))
    }
}

sealed abstract class DerivedInstances extends DerivedInstances1 {
  implicit def bitraverseTraverse[F[_, _], A](implicit F: Bitraverse[F]) =
    new Traverse[F[A, ?]] {
      def foldLeft[B, C](fa: F[A, B], b: C)(f: (C, B) ⇒ C) =
        F.bifoldLeft(fa, b)((c, _) => c, f)

      def foldRight[B, C](fa: F[A, B], lb: Eval[C])(f: (B, Eval[C]) ⇒ Eval[C]) =
        F.bifoldRight(fa, lb)((a, c) => c, f)

      def traverse[G[_], B, C](fa: F[A, B])(f: (B) ⇒ G[C])(implicit arg0: Applicative[G]) =
        F.bitraverse(fa)(arg0.pure, f)
    }
}

sealed abstract class DerivedInstances1 extends DerivedInstances0 {
  implicit def bifoldableFoldable[F[_, _], A](implicit F: Bifoldable[F]) =
    new Foldable[F[A, ?]] {
      def foldLeft[B, C](fa: F[A, B], b: C)(f: (C, B) ⇒ C) =
        F.bifoldLeft(fa, b)((c, _) => c, f)

      def foldRight[B, C](fa: F[A, B], lb: Eval[C])(f: (B, Eval[C]) ⇒ Eval[C]) =
        F.bifoldRight(fa, lb)((a, c) => c, f)
    }
}

sealed abstract class DerivedInstances0 {
  implicit def bifunctorFunctor[F[_, _], A](implicit F: Bifunctor[F]) =
    new Functor[F[A, ?]] {
      def map[B, C](fa: F[A, B])(f: B => C) = F.bimap(fa)(x => x, f)
    }
}
