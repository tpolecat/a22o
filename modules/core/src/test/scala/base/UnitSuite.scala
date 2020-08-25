// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package base

import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class UnitSuite extends ScalaCheckSuite {
  import Parser.{ const, unit }

  property("unit == const(())") {
    unit == const(())
  }

}
