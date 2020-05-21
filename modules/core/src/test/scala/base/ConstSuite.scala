// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package base

import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class ConstSuite extends ScalaCheckSuite {

  property("const consumes no input") {
    forAll { (s: String, n: Int) =>
      val p = Parser.const(n)
      val r = p.parse(s).toRemainingAndEither
      assertEquals(r, (s, Right(n)))
    }
  }

}
