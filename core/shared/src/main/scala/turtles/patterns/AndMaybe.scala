/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.patterns

import slamdata.Predef.{Eq => _, _}
import turtles._

import cats._
import cats.implicits._
import monocle.Iso

sealed abstract class AndMaybe[A, B] {
  def head: A = this match {
    case Indeed(a, _) => a
    case Only(a)      => a
  }

  def tailOption: Option[B] = this match {
    case Indeed(_, b) => b.some
    case Only(_)      => none
  }
}
final case class Indeed[A, B](h: A, t: B) extends AndMaybe[A, B]
final case class Only[A, B](a: A)         extends AndMaybe[A, B]

object AndMaybe extends AndMaybeInstances {
  def envTIso[A, B] = Iso[AndMaybe[A, B], EnvT[A, Option, B]] {
    case Indeed(h, t) => EnvT((h, t.some))
    case Only(h)      => EnvT((h, none))
  } (envt => envt.lower.fold[AndMaybe[A, B]](Only(envt.ask))(Indeed(envt.ask, _)))

  def find[A](p: A => Boolean): Algebra[AndMaybe[A, ?], Option[A]] =
    l => if (p(l.head)) l.head.some else l.tailOption.flatten
}

sealed abstract class AndMaybeInstances {
  implicit val bitraverse: Bitraverse[AndMaybe] =
    new Bitraverse[AndMaybe] {
      def bitraverse[G[_]: Applicative, A, B, C, D](
        fab: AndMaybe[A, B])(
        f: A => G[C], g: B => G[D]) =
        fab match {
          case Indeed(a, b) => (f(a), g(b)).mapN(Indeed(_, _))
          case Only(a)      => f(a) map (Only(_))
        }

      def bifoldLeft[A, B, C](
        fab: AndMaybe[A, B], c: C)(
        f: (C, A) => C, g: (C, B) => C) =
        fab match {
          case Indeed(a, b) => g(f(c, a), b)
          case Only(a)      => f(c, a)
        }

      def bifoldRight[A, B, C](
        fab: AndMaybe[A, B], c: Eval[C])(
        f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]) =
        fab match {
          case Indeed(a, b) => f(a, g(b, c))
          case Only(a)      => f(a, c)
        }
    }

  implicit def equal[A: Eq]: Delay[Eq, AndMaybe[A, ?]] =
    new Delay[Eq, AndMaybe[A, ?]] {
      def apply[B](eql: Eq[B]) = {
        implicit val eqlB: Eq[B] = eql
        Eq.by {
          case Indeed(a, b) => (a, b).asRight[A]
          case Only(a)      => a.asLeft[(A, B)]
        }
      }
    }

  implicit def show[A: Show]: Delay[Show, AndMaybe[A, ?]] =
    new Delay[Show, AndMaybe[A, ?]] {
      def apply[B](show: Show[B]) =
        Show.show {
          case Indeed(a, b) => a.show |+| "::" |+| show.show(b)
          case Only(a)      => a.show
        }
    }
}
