// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package base

import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class UnitSuite extends ScalaCheckSuite {

  property("unit consumes no input") {
    forAll { (s: String) =>
      val r = Parser.unit.parse(s).toRemainingAndEither
      assertEquals(r, (s, Right(())))
    }
  }

}
