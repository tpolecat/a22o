package a22o
package base

import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class ConstSuite extends ScalaCheckSuite {

  property("const consumes no input") {
    forAll { (s: String, n: Int) =>
      val p = Parser.const(n)
      val r = p.parse(s)
      assertEquals(r, (s, Right(n)))
    }
  }

}
