/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.patterns

import slamdata.Predef.{Eq => _, _}
import turtles._

import cats._
import cats.implicits._

/** Represents diffs of recursive data structures.
  */
sealed abstract class Diff[T[_[_]], F[_], A]
final case class Same[T[_[_]], F[_], A](ident: T[F]) extends Diff[T, F, A]
final case class Similar[T[_[_]], F[_], A](ident: F[A]) extends Diff[T, F, A]
final case class Different[T[_[_]], F[_], A](left: T[F], right: T[F])
    extends Diff[T, F, A]
final case class LocallyDifferent[T[_[_]], F[_], A](left: F[A], right: F[Unit])
    extends Diff[T, F, A]
final case class Inserted[T[_[_]], F[_], A](right: F[A])
    extends Diff[T, F, A]
final case class Deleted[T[_[_]], F[_], A](left: F[A])
    extends Diff[T, F, A]
final case class Added[T[_[_]], F[_], A](right: T[F])
    extends Diff[T, F, A]
final case class Removed[T[_[_]], F[_], A](left: T[F])
    extends Diff[T, F, A]

object Diff extends DiffInstances

sealed abstract class DiffInstances extends DiffInstances0 {
  implicit def traverse[T[_[_]], F[_]: Traverse]: Traverse[Diff[T, F, ?]] =
    new Traverse[Diff[T, F, ?]] {
      def traverse[G[_], A, B](
        fa: Diff[T, F, A])(
        f: A => G[B])(
        implicit G: Applicative[G]) =
        fa match {
          case Same(ident) => G.pure(Same(ident))
          case Similar(ident) => ident.traverse(f).map(Similar(_))
          case Different(left, right) =>
            G.pure(Different(left, right))
          case LocallyDifferent(left, right) =>
            left.traverse(f).map(LocallyDifferent(_, right))
          case Inserted(right) => right.traverse(f).map(Inserted(_))
          case Deleted(left) => left.traverse(f).map(Deleted(_))
          case Added(right) => G.pure(Added(right))
          case Removed(left) => G.pure(Removed(left))
        }

      def foldLeft[A, B](fa: Diff[T, F, A],b: B)(f: (B, A) => B) =
        fa match {
          case Same(_) => b
          case Similar(ident) => ident.foldLeft(b)(f)
          case Different(_, _) => b
          case LocallyDifferent(left, right) => left.foldLeft(b)(f)
          case Inserted(right) => right.foldLeft(b)(f)
          case Deleted(left) => left.foldLeft(b)(f)
          case Added(right) => b
          case Removed(left) => b
        }

      def foldRight[A, B](
        fa: Diff[T, F, A], b: Eval[B])(
        f: (A, Eval[B]) => Eval[B]) =
        fa match {
          case Same(_) => b
          case Similar(ident) => ident.foldRight(b)(f)
          case Different(_, _) => b
          case LocallyDifferent(left, right) => left.foldRight(b)(f)
          case Inserted(right) => right.foldRight(b)(f)
          case Deleted(left) => left.foldRight(b)(f)
          case Added(right) => b
          case Removed(left) => b
        }
    }

  implicit def equal[T[_[_]], F[_]](implicit T: Eq[T[F]], F: Delay[Eq, F]): Delay[Eq, Diff[T, F, ?]] =
    new Delay[Eq, Diff[T, F, ?]] {
      def apply[α](eq: Eq[α]) =
        Eq.instance((a, b) => (a, b) match {
          case (Same(i1), Same(i2)) => T.eqv(i1, i2)
          case (Similar(i1), Similar(i2)) => F(eq).eqv(i1, i2)
          case (Different(l1, r1), Different(l2, r2)) =>
            T.eqv(l1, l2) && T.eqv(r1, r2)
          case (LocallyDifferent(l1, r1), LocallyDifferent(l2, r2)) =>
            F(eq).eqv(l1, l2) && F(Eq[Unit]).eqv(r1, r2)
          case (Inserted(r1), Inserted(r2)) => F(eq).eqv(r1, r2)
          case (Deleted(l1), Deleted(l2)) => F(eq).eqv(l1, l2)
          case (Added(r1), Added(r2)) => T.eqv(r1, r2)
          case (Removed(l1), Removed(l2)) => T.eqv(l1, l2)
          case (_, _) => false
        })
    }

  implicit def show[T[_[_]], F[_]: Functor: Foldable](
    implicit T: Show[T[F]], F: Delay[Show, F]):
      Delay[Show, Diff[T, F, ?]] =
    new Delay[Show, Diff[T, F, ?]] {
      def apply[α](s: Show[α]) = Show.show {
        case Same(_) => "..."
        case Similar(x) => F(s).show(x)
        case Different(l, r) =>
          "vvvvvvvvv left  vvvvvvvvv\n" |+|
            T.show(l) |+|
            "\n=========================\n" |+|
            T.show(r) |+|
            "\n^^^^^^^^^ right ^^^^^^^^^"
        case Inserted(x) =>  "+++> " |+| F(s).show(x)
        case Deleted(x) => "<--- " |+| F(s).show(x)
        case Added(x) => "+++> " |+| T.show(x)
        case Removed(x) => "<--- " |+| T.show(x)
        case LocallyDifferent(l, r) =>
          F(s).show(l) |+| " <=/=> " |+| F(Show[Unit]).show(r)
      }
    }
}

sealed abstract class DiffInstances1 {
  implicit def foldable[T[_[_]], F[_]: Foldable]: Foldable[Diff[T, F, ?]] =
    new Foldable[Diff[T, F, ?]] {
      def foldLeft[A, B](fa: Diff[T, F, A], b: B)(f: (B, A) => B) =
        fa match {
          case Same(_) => b
          case Similar(ident) => ident.foldLeft(b)(f)
          case Different(_, _) => b
          case LocallyDifferent(left, right) => left.foldLeft(b)(f)
          case Inserted(right) => right.foldLeft(b)(f)
          case Deleted(left) => left.foldLeft(b)(f)
          case Added(right) => b
          case Removed(left) => b
        }

      def foldRight[A, B](
        fa: Diff[T, F, A], b: Eval[B])(
        f: (A, Eval[B]) => Eval[B]) =
        fa match {
          case Same(_) => b
          case Similar(ident) => ident.foldRight(b)(f)
          case Different(_, _) => b
          case LocallyDifferent(left, right) => left.foldRight(b)(f)
          case Inserted(right) => right.foldRight(b)(f)
          case Deleted(left) => left.foldRight(b)(f)
          case Added(right) => b
          case Removed(left) => b
        }
    }
}

sealed abstract class DiffInstances0 {
  implicit def functor[T[_[_]], F[_]: Functor]: Functor[Diff[T, F, ?]] =
    new Functor[Diff[T, F, ?]] {
      def map[A, B](fa: Diff[T, F, A])(f: A => B): Diff[T, F, B] =
        fa match {
          case Same(ident)                   => Same(ident)
          case Similar(ident)                => Similar(ident.map(f))
          case Different(left, right)        => Different(left, right)
          case LocallyDifferent(left, right) =>
            LocallyDifferent(left.map(f), right)
          case Inserted(right)               => Inserted(right.map(f))
          case Deleted(left)                 => Deleted(left.map(f))
          case Added(right)                  => Added(right)
          case Removed(left)                 => Removed(left)
        }
    }
}
