// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package base

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Gen._

class SkipSuite extends ScalaCheckSuite {

  property("positive skip consumes chars") {
    forAll(alphaNumStr.filter(_.length > 0), posNum[Int]) { (s, n) =>
      val nʹ = n % s.length
      val p = Parser.skip(nʹ)
      val r = p.parse(s).toRemainingAndEither
      assertEquals(r, (s drop nʹ, Right(())))
    }
  }

  property("negative skip fails") {
    forAll(alphaNumStr, posNum[Short]) { (s, n) =>
      val nʹ = -(n.toInt + 1)
      val p = Parser.skip(nʹ)
      val r = p.parse(s).toRemainingAndEither
      assertEquals(r, (s, Left("skip: negative length")))
    }
  }

  // TODO: insufficient input

  property("skip(0) == unit") {
    Parser.skip(0) == Parser.unit
  }

  property("skip(n).void == skip(n)") {
    forAll { (n: Int) =>
      Parser.skip(n).void == Parser.skip(n)
    }
  }

  property("skip(n).inputText == take(n)") {
    forAll { (n: Int) =>
      Parser.skip(n).inputText == Parser.take(n)
    }
  }

  property("skip(0) == unit") {
    Parser.skip(0) == Parser.unit
  }

}
