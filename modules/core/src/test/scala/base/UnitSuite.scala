package a22o
package base

import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class UnitSuite extends ScalaCheckSuite {
  import Parser._

  property("unit consumes no input") {
    forAll { (s: String) =>
      val r = unit.parse(s)
      assertEquals(r, (s, Right(())))
    }
  }

}
