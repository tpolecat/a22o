// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package int

import a22o.Parser.int
import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class IntSuite extends ScalaCheckSuite {

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
         .withMinSuccessfulTests(1000)

  property("parse any int") {
    forAll { (n: Int) =>
      assertEquals(int.parse(n.toString).toRemainingAndEither, (("", Right(n))))
    }
  }

  property("parse any int, with suffix") {
    forAll { (n: Int) =>
      val suffix = "abc"
      assertEquals(int.parse(n.toString + suffix).toRemainingAndEither, ((suffix, Right(n))))
    }
  }

  property("over/underflow") {
    forAll { (n: Long) =>
      val z = int.parse(n.toString).toEither
      z match {
        case Right(m) => assertEquals(n.toInt, m)
        case Left(s)  =>
          assert(n < Int.MinValue.toLong || n > Int.MaxValue.toLong)
          assert(s == "Integer over/underflow.")
      }
    }
  }

  test("empty string") {
    assertEquals(int.parse("").toEither, Left("Expected sign or digit."))
  }

  test("just a sign") {
    assertEquals(int.parse("+").toEither, Left("Expected digit."))
    assertEquals(int.parse("-").toEither, Left("Expected digit."))
    assertEquals(int.parse("+suffix").toEither, Left("Expected digit."))
    assertEquals(int.parse("-suffix").toEither, Left("Expected digit."))
  }

  test("boundary cases") {
    assertEquals(int.parse(Int.MinValue.toString).toEither, Right(Int.MinValue))
    assertEquals(int.parse(Int.MaxValue.toString).toEither, Right(Int.MaxValue))
    assert(int.parse((Int.MinValue.toLong - 1).toString).toEither.isLeft)
    assert(int.parse((Int.MaxValue.toLong + 1).toString).toEither.isLeft)
  }

}