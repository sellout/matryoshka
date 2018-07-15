/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
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
