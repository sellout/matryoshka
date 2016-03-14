/*
 * Copyright 2014 - 2015 SlamData Inc.
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

package matryoshka

import scalaz.Leibniz.{ ===, refl }

/** Unapply for a typeclass of shape TC[_[_[_]]]. */
trait Funapply[TC[_[_[_]]], MF, FMF] {

  /** The type constructor */
  type M[_[_]]

  /** The type that `M` was applied to */
  type F[_]

  /** The instance of the type class */
  def TC: TC[M]

  /** Evidence that MF =:= M[F] */
  def leibniz: MF === M[F]

  /** Evidence that FMF =:= F[M[F]] */
  def leibnizF: FMF === F[M[F]]

}

object Funapply {

  def apply[TC[_[_[_]]], MF, FMF](implicit U: Funapply[TC, MF, FMF]): U.type {
    type M[X[_]] = U.M[X]
    type F[X] = U.F[X]
  } = U

  implicit def funapply0[TC[_[_[_]]], M0[_[_]], F0[_]](implicit TC0: TC[M0]): Funapply[TC, M0[F0], F0[M0[F0]]] {
    type M[X[_]] = M0[X]
    type F[X] = F0[X]
  } = new Funapply[TC, M0[F0], F0[M0[F0]]] {
    type M[X[_]] = M0[X]
    type F[X] = F0[X]
    def TC: TC[M] = TC0
    def leibniz = refl
    def leibnizF = refl
  }

  implicit def funapply1[TC[_[_[_]]], M0[_[_], _], F0[_], A](implicit TC0: TC[M0[?[_], A]]): Funapply[TC, M0[F0, A], F0[M0[F0, A]]] {
    type M[X[_]] = M0[X, A]
    type F[X] = F0[X]
  } = new Funapply[TC, M0[F0, A], F0[M0[F0, A]]] {
    type M[X[_]] = M0[X, A]
    type F[X] = F0[X]
    def TC: TC[M] = TC0
    def leibniz = refl
    def leibnizF = refl
  }

}