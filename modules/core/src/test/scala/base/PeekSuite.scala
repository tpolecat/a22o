// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package base

import org.scalacheck.Prop._
import a22o.arb._

class PeekSuite extends ParserSuite {

  property("p.peek is the same as p but consumes no input") {
    forAll(parserWithInput) { case (p, s0) =>
      val ( _, r1) = p.parse(s0).toRemainingAndEither
      val (s2, r2) = p.peek.parse(s0).toRemainingAndEither
      assertEquals(r1, r2)
      assertEquals(s2, s0)
    }
  }

  property("p.peek <~ string(s) sameParserAs p") {
    forAll(parserWithInput) { case (p, s) =>
      val pʹ = p.peek <~ Parser.string(s)
      assertEquals(pʹ.parseAll(s), p.parseAll(s))
    }
  }

}
