/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.instances

import slamdata.Predef.{Eq => _, _}
import turtles._
import turtles.data._
import turtles.derived._
import turtles.implicits._
import turtles.patterns._

import cats._
import cats.implicits._
import monocle.Prism

/** This package provides instances of various common data structures
  * implemented explicitly as fixed-points.
  */
package object fixedpoint {

  /** Natural numbers represented as the least fixed-point of [[scala.Option]].
    */
  type Nat = Mu[Option]

  object Nat {
    def zero[N](implicit N: Steppable.Aux[N, Option]) = none[N].embed

    def succ[N](prev: N)(implicit N: Steppable.Aux[N, Option]) =
      prev.some.embed

    def one[N](implicit N: Steppable.Aux[N, Option]) = succ(zero)

    val fromInt: CoalgebraM[Option, Option, Int] =
      x => if (x < 0) None else Some(if (x > 0) (x - 1).some else None)

    // NB: This isn’t defined via `AlgebraPrism` because it only holds across a
    //     recursive structure.
    def intPrism[T]
      (implicit
        TS: Steppable.Aux[T, Option],
        TR: Recursive.Aux[T, Option],
        TC: Corecursive.Aux[T, Option]) =
      Prism[Int, T](_.anaM[T](fromInt))(_.cata(height))
  }

  implicit class SteppableOptionOps[T]
    (self: T)
    (implicit T: Steppable.Aux[T, Option]) {
    def succ = Nat.succ(self)
  }

  implicit class RecursiveOptionOps[T]
    (self: T)
    (implicit T: Recursive.Aux[T, Option]) {
    def toInt = self.cata(height)

    def +(other: T)(implicit TS: Steppable.Aux[T, Option]) = other.cata[T] {
      case None => self
      case o    => o.embed
    }
  }

  /** The dual of [[Nat]], a potentially-infinite number. */
  type Conat = Nu[Option]
  object Conat {
    /** A representation of infinity, as a non-terminating corecursive process */
    def inf[N](implicit N: Corecursive.Aux[N, Option]): N = ().ana[N](_.some)
  }

  type Free[F[_], A]   = Mu[CoEnv[A, F, ?]]
  type Cofree[F[_], A] = Mu[EnvT[A, F, ?]]
  type List[A]         = Mu[ListF[A, ?]]
  object List {
    def apply[A](elems: A*) =
      elems.toList.ana[Mu[ListF[A, ?]]](ListF.listIso[A].reverseGet)

    def tuple[A](elem: => A) = λ[Option ~> ListF[A, ?]] {
      case None    => NilF()
      case Some(b) => ConsF(elem, b)
    }

    def forget[A] = λ[ListF[A, ?] ~> Option] {
      case NilF()      => None
      case ConsF(_, t) => t.some
    }

    object fill {
      def apply[L] = new PartiallyApplied[L]
      class PartiallyApplied[L] {
        def apply[N, A]
          (n: N)
          (elem: => A)
          (implicit
            NS: Steppable.Aux[N, Option],
            NR: Recursive.Aux[N, Option],
            LS: Steppable.Aux[L, ListF[A, ?]],
            LC: Corecursive.Aux[L, ListF[A, ?]])
            : L =
          n.transAna[L](tuple(elem)(_))
      }
    }
  }

  implicit class SteppableListFOps[T, A]
    (self: T)
    (implicit T: Steppable.Aux[T, ListF[A, ?]]) {
    def headOption: Option[A] = self.project.headOption
    def tailOption: Option[T] = self.project.tailOption
  }

  // FIXME: This implicit conversion seems to not get found, so we specialize
  //        `T` below.
  implicit class RecursiveListFOps[T, A]
    (self: T)
    (implicit T: Recursive.Aux[T, ListF[A, ?]]) {
    def find(cond: A => Boolean): Option[A] = self.cata(ListF.find(cond))
    def length: Int = self.cata(height)
  }

  implicit class CorecursiveListFOps[T, A]
    (self: T)
    (implicit T: Corecursive.Aux[T, ListF[A, ?]]) {
    def take[N]
      (i: N)
      (implicit N: Steppable.Aux[N, Option], TS: Steppable.Aux[T, ListF[A, ?]])
        : T =
      (i, self).ana[T](ListF.takeUpTo)
  }

  implicit def steppableTListFOps[T[_[_]]: SteppableT, A](self: T[ListF[A, ?]])
      : SteppableListFOps[T[ListF[A, ?]], A] =
    new SteppableListFOps[T[ListF[A, ?]], A](self)

  implicit def recursiveTListFOps[T[_[_]]: RecursiveT, A](self: T[ListF[A, ?]])
      : RecursiveListFOps[T[ListF[A, ?]], A] =
    new RecursiveListFOps[T[ListF[A, ?]], A](self)

  implicit def corecursiveTListFOps[T[_[_]]: CorecursiveT, A](self: T[ListF[A, ?]])
      : CorecursiveListFOps[T[ListF[A, ?]], A] =
    new CorecursiveListFOps[T[ListF[A, ?]], A](self)

  implicit def recursiveTListFFoldable[T[_[_]]: RecursiveT]: Foldable[λ[α => T[ListF[α, ?]]]] =
    new Foldable[λ[α => T[ListF[α, ?]]]] {
      override def foldMap[A, B: Monoid](fa: T[ListF[A, ?]])(f: A ⇒ B) =
        fa.cata[B] {
          case NilF()      => Monoid[B].empty
          case ConsF(a, b) => f(a) |+| b
        }

      def foldLeft[A, B](fa: T[ListF[A, ?]], z: B)(f: (B, A) ⇒ B) =
        fa.cata[B] {
          case NilF()      => z
          case ConsF(a, b) => f(b, a)
        }

      def foldRight[A, B](fa: T[ListF[A, ?]], z: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]) =
        fa.cata[Eval[B]] {
          case NilF()      => z
          case ConsF(a, b) => f(a, b)
        }
    }

  implicit def recursiveListFMonoid[T, A]
    (implicit
      TS: Steppable.Aux[T, ListF[A, ?]],
      TR: Recursive.Aux[T, ListF[A, ?]])
      : Monoid[T] =
    new Monoid[T] {
      def empty = NilF[A, T]().embed
      def combine(f1: T, f2: T) = f1.cata[T] {
        case NilF() => f2
        case cons   => cons.embed
      }
    }

  /** A lazy (potentially-infinite) list.
    */
  type Colist[A] = Nu[ListF[A, ?]]

  type NonEmptyList[A] = Mu[AndMaybe[A, ?]]
  type NonEmptyColist[A] = Nu[AndMaybe[A, ?]]

  implicit class SteppableAndMaybeOps[T, A]
    (self: T)
    (implicit T: Steppable.Aux[T, AndMaybe[A, ?]]) {
    def toPossiblyEmpty[L]
      (implicit
        LS: Steppable.Aux[L, ListF[A, ?]],
        LC: Corecursive.Aux[L, ListF[A, ?]]) =
      self.transApo[L, ListF[A, ?]] {
        case Indeed(a, b) => ConsF(a, b.asRight)
        case Only(a)      => ConsF(a, NilF[A, L]().embed.asLeft)
      }
  }

  /** A true stream – infinite.
    */
  type Stream[A] = Nu[(A, ?)]

  object Stream {
    def matchesFirst[A, B](cond: A => Boolean) =
      λ[(A, ?) ~> Either[A, ?]] {
        case (h, t) => if (cond(h)) h.asLeft else t.asRight
      }

    def take[N, T, A]
      (implicit N: Steppable.Aux[N, Option], T: Steppable.Aux[T, (A, ?)])
        : Coalgebra[ListF[A, ?], (N, T)] = {
      case (n, s) =>
        n.project.fold[ListF[A, (N, T)]](
          NilF())(
          prev => {
            val pair = s.project
            ConsF(pair._1, (prev, pair._2))
          })
    }

    /** Colists are simply streams that may terminate, so a stream is easily
      * converted to a Colist that doesn’t terminate.
      */
    def toConsF[A] = λ[(A, ?) ~> ListF[A, ?]](p => ConsF(p._1, p._2))

    def toIndeed[A] = λ[(A, ?) ~> AndMaybe[A, ?]](p => Indeed(p._1, p._2))
  }

  implicit class SteppableTuple2Ops[T, A]
    (self: T)
    (implicit T: Steppable.Aux[T, (A, ?)]) {
    def head: A = self.project._1

    def tail: T = self.project._2

    /** Colists are simply streams that may terminate, so a stream is easily
      * converted to a Colist that doesn’t terminate.
      */
    def toColist[L]
      (implicit
        LS: Steppable.Aux[L, ListF[A, ?]],
        LC: Corecursive.Aux[L, ListF[A, ?]])
        : L =
      self.transAna[L](Stream.toConsF(_))

    def toNEColist[L]
      (implicit
        LS: Steppable.Aux[L, AndMaybe[A, ?]],
        LC: Corecursive.Aux[L, AndMaybe[A, ?]])
        : L =
      self.transAna[L](Stream.toIndeed(_))

    object take {
      def apply[L] = new PartiallyApplied[L]
      class PartiallyApplied[L] {
        def apply[N]
          (n: N)
          (implicit
            N: Steppable.Aux[N, Option],
            L: Corecursive.Aux[L, ListF[A, ?]])
            : L =
          (n, self).ana[L](Stream.take[N, T, A])
      }
    }
  }

  implicit def SteppableTTuple2Ops[T[_[_]]: SteppableT, A](self: T[(A, ?)])
      : SteppableTuple2Ops[T[(A, ?)], A] =
    new SteppableTuple2Ops[T[(A, ?)], A](self)

  implicit class CorecursiveTuple2Ops[T, A]
    (self: T)
    (implicit T: Corecursive.Aux[T, (A, ?)]) {
    /** Drops exactly `n` elements from the stream.
      * This doesn’t expose the Coalgebra because it returns
      * `Either[Stream, Stream]`, which isn’t the type of `drop`.
      */
    def drop[N]
      (n: N)
      (implicit N: Steppable.Aux[N, Option], TS: Steppable.Aux[T, (A, ?)])
        : T =
      (n, self).anaM[T] {
        case (r, stream) =>
          r.project.fold[Either[T, (A, (N, T))]](
            stream.asLeft)(
            prev => stream.project.map((prev, _)).asRight)
      }.merge
  }

  implicit def CorecursiveTTuple2Ops[T[_[_]]: CorecursiveT, A](self: T[(A, ?)])
      : CorecursiveTuple2Ops[T[(A, ?)], A] =
    new CorecursiveTuple2Ops[T[(A, ?)], A](self)

  /** Encodes a function that may diverge.
    */
  type Partial[A] = Nu[Either[A, ?]]

  object Partial {
    /** A partial function that immediately evaluates to the provided value.
      */
    def now[A](a: A): Partial[A] = a.asLeft[Nu[Either[A, ?]]].embed

    def later[A](partial: Partial[A]): Partial[A] = partial.asRight[A].embed

    def delay[A](a: A): Option ~> Either[A, ?] =
      λ[Option ~> Either[A, ?]](_.toRight(a))

    /** Canonical function that diverges.
      */
    def never[A]: Partial[A] = ().ana[Nu[Either[A, ?]]](_.asRight[A])

    /** This instance is not implicit, because it potentially runs forever.
      */
    def equal[A: Eq]: Eq[Partial[A]] =
      Eq.instance((a, b) => (a ≈ b).unsafePerformSync)

    def fromOption[A](opt: Option[A]): Partial[A] = opt.fold(never[A])(now)

    def fromPartialFunction[A, B](pf: PartialFunction[A, B]):
        A => Partial[B] =
      pf.lift >>> fromOption
  }

  implicit def recursiveTEitherFoldable[T[_[_]]: RecursiveT]
      : Foldable[λ[α => T[Either[α, ?]]]] =
    new Foldable[λ[α => T[Either[α, ?]]]] {
      override def foldMap[A, B: Monoid](fa: T[Either[A, ?]])(f: A ⇒ B) =
        fa.cata[B](_.leftMap(f).merge)

      def foldLeft[A, B](fa: T[Either[A, ?]], z: B)(f: (B, A) ⇒ B) =
        fa.cata[B](_.leftMap(f(z, _)).merge)

      def foldRight[A, B](fa: T[Either[A, ?]], z: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]) =
        fa.cata[Eval[B]](_.leftMap(f(_, z)).merge)
    }

  implicit val partialMonad: Monad[Partial] = new Monad[Partial] {
    def pure[A](a: A) = Partial.now(a)

    def flatMap[A, B](fa: Partial[A])(f: A => Partial[B]) =
      fa.cata[Partial[B]](_.leftMap(f).merge)

    @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
    def tailRecM[A, B](a: A)(f: A => Partial[Either[A,B]]): Partial[B] =
      f(a).flatMap(_.fold(tailRecM(_)(f), pure))
  }

  implicit class PartialOps[A](self: Partial[A]) {
    def step: Either[A, Partial[A]] = self.project

    /** Returns `left` if the result was found within the given number of steps.
      */
    def runFor[N](steps: N)(implicit N: Steppable.Aux[N, Option])
        : Either[A, Partial[A]] =
      (steps, self).anaM[Partial[A]] {
        case (r, p) => r.project.fold[Either[Either[A, Partial[A]], Either[A, (N, Partial[A])]]](
          p.project.asLeft)(
          prev => p.project.bimap(_.asLeft, (prev, _).asRight))
      }.map(_.step).merge

    /** Run to completion (if it completes).
      */
    // TODO: Can we do this stack-safely with a fold?
    //         _.cata(_.merge)                                          // blows up
    //         _.cataM[Free.Trampoline](p => Trampoline.delay(p.merge)) // still blows up
    //         _.hyloM[Free.Trampoline](..., p.project.point)           // takes forever
    @tailrec final def unsafePerformSync: A = self.project match {
      case Left(a)  => a
      case Right(p) => p.unsafePerformSync
    }

    // TODO: Would be nice to have this in ApplicativeOps
    /** If two `Partial`s eventually have the same value, then they are
      * equivalent.
      */
    def ≈(that: Partial[A])(implicit A: Eq[A]): Partial[Boolean] =
      (self, that).mapN(_ === _)
  }
}
