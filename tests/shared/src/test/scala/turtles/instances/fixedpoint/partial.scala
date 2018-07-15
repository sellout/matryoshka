/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.instances.fixedpoint

import org.scalacheck.{ Arbitrary, Gen }
import slamdata.Predef.{ Int, Left, Unit }
import turtles._
import turtles.derived._
import turtles.implicits._
import turtles.helpers.TurtlesSuite

class PartialSpec extends TurtlesSuite {

  /** For testing cases that should work with truly diverging functions. */
  def sometimesNeverGen[A: Arbitrary]: Gen[Partial[A]] =
    Gen.oneOf(Arbitrary.arbitrary[Partial[A]], Gen.const(Partial.never[A]))

  // checkAll("Partial[Int]", EqTests[Partial[Int]].eqv(Partial.equal, implicitly))
  // checkAll("Partial", MonadTests[Partial].monad(implicitly, implicitly, implicitly, implicitly, Partial.equal))
  // checkAll("Partial", FoldableTests[Partial].foldable)

  // https://en.wikipedia.org/wiki/McCarthy_91_function
  def mc91(n: Int): Partial[Int] =
    if (n > 100) Partial.now(n - 10)
    else mc91(n + 11) >>= mc91

  test("never should always have more steps") {
    forAll { (i: Conat) =>
      Partial.never[Int].runFor(i) shouldBe 'right
    }
  }

  test("runFor shuold return now immediately") {
    Partial.now(13).runFor(Nat.zero[Nat]) shouldEqual Left(13)
  }

  test("runFor should return a value when it runs past the end") {
    forAll { (i: Conat) =>
      i.transAna[Partial[Int]](Partial.delay(7)(_)).runFor(i) shouldEqual Left(7)
    }
  }

  // test("runFor should return after multiple runs") {
  //   forAll { (a: Conat, b: Conat) =>
  //     b > Nat.zero[Conat] ==> {
  //       val first = (a + b).transAna[Partial[Int]](Partial.delay(27)(_)).runFor(a)
  //       first shouldBe 'isRight
  //       first.flatMap(_.runFor(b)) shouldEqual Left(27)
  //     }
  //   }
  // }

  test("runFor still pending one short") {
    forAll { (a: Conat) =>
      val first = (a + Nat.one[Conat]).transAna[Partial[Int]](Partial.delay(27)(_)).runFor(a)
      first shouldBe 'right
      first.flatMap(_.runFor(a + Nat.one[Conat])) shouldEqual Left(27)
    }
  }

  test("runFor should return exactly at the end") {
    forAll { (n: Conat, i: Int) =>
      n.transAna[Partial[Int]](Partial.delay(i)(_)).runFor(n) shouldEqual Left(i)
    }
  }

  test("unsafePerformSync should return now immediately") {
    Partial.now(12).unsafePerformSync should === (12)
  }

  test("unsafePerformSync should return a value when it gets to the end") {
    Partial.later(Partial.later(Partial.now(3))).unsafePerformSync should === (3)
  }

  // NB: This test will depend on the size of your stack, you may have to
  //     increase the initial value on larger stacks.
  test("unsafePerformSync should return a value well after stack would overflow") {
    100000000.ana[Partial[Unit]](i => if (i === 0) ().asLeft else (i - 1).asRight)
      .unsafePerformSync should === (())
  }

  // NB: Reduced from -97000 with Scalaz.
  val mc91LowerBound = -90000

  // NB: This is because the following test doesn't always get close to the
  //     lower bound, so we make sure changes don't make things worse.
  test("unsafePerformSync should check lower bound of mc91") {
    mc91(mc91LowerBound).unsafePerformSync should === (91)
  }

  // TODO: Should work with any Int, but stack overflows on big negatives.
  // test("unsafePerformSync should always terminate with mc91") {
  //   forAll { (n: Int) => n > mc91LowerBound ==>
  //     (mc91(n).unsafePerformSync should === (if (n <= 100) 91 else n - 10))
  //   }
  // }

}
