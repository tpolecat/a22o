// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package base

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import a22o.arb._

class SanitySuite extends ScalaCheckSuite {

  property("generated parsers and input succeed and fail as expected") {
    forAll(parserWithInputAndExpectation) { case (p, s, b) =>
      assertEquals(p.parseAll(s).isRight, b)
    }
  }

  property("void succeeds/fails the same as the underlying paser") {
    forAll(parserWithInput) { case (p, s) =>
      assertEquals(p.parseAll(s).map(_ => ()), p.void.parseAll(s))
    }
  }

}
