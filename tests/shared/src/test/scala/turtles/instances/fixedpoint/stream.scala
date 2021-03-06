/* Copyright 2014–2018 SlamData Inc. and Greg Pfeil.
 * Licensed under the Apache License, Version 2.0.
 * See https://github.com/sellout/turtles#copyright for details.
 */

package turtles.instances.fixedpoint

import slamdata.Predef.{Eq => _, _}
import turtles._
import turtles.helpers.TurtlesSuite
import turtles.implicits._

class StreamSpec extends TurtlesSuite {

  // Infinite sequence of Fibonacci numbers (at least until they overflow int32)
  val fib = (1, 0).ana[Stream[Int]](binarySequence(_ + _))

  // Generates an infinite stream of the carrier value.
  def constantly[A]: Coalgebra[(A, ?), A] = i => (i, i)

  test("fib should begin with 1") {
    fib.head should === (1)
  }

  test("fib should lazily generate the correct sequence") {
    5.anaM[Nat](Nat.fromInt).map(fib.drop(_).head) should === (8.some)
  }

  // test("fib should have a proper prefix") {
  //   5.anaM[Nat](Nat.fromInt).map(fib.take[List[Int]](_)) should === (List(1, 1, 2, 3, 5).some)
  // }
  //
  // test("fib should get a subsequence") {
  //   (10.anaM[Nat](Nat.fromInt), 5.anaM[Nat](Nat.fromInt)).mapN((d, t) =>
  //     fib.drop(d).take[List[Int]](t)) should === (List(89, 144, 233, 377, 610).some)
  // }

  test("constantly shuold begin with the given value") {
    forAll { (i: Int) =>
      i.ana[Stream[Int]](constantly).head should === (i)
    }
  }

  // FIXME: These two blow up the stack with much larger inputs

  test("constantly shuold have the given value at an arbitrary point") {
    forAll { (i: Int) =>
      350.anaM[Nat](Nat.fromInt).map(
        i.ana[Stream[Int]](constantly).drop(_).head) should === (i.some)
    }
  }

  // test("constantly shuold have subsequence of the given value") {
  //   forAll { (n: Nat, i: Int, t: Int) =>
  //     i.ana[Stream[Int]](constantly).take[List[Int]](n) should === (List.fill[List[Int]](n)(i))
  //   }
  // }

}
