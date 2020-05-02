// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package allocation
package parser

import a22o.parser._
import cats.implicits._

class ParserSuite extends AllocationSuite with Text {

  test("map (depends on argument)") {
    val p = char('a').map(c => Some(c))
    p.assertAllocation("abc", "def")(
      "scala/Some" -> 1
    )
  }

  test("map + as (none)") {
    val p = char('a').map(c => Some(c))
    p.as(None).void.assertNoAllocation("abc", "def")
  }

  test("applicative composition (none if args don't allocate)".ignore) {
    val p = (letter, digit, whitespace, digit).mapN { (_, _, _, _) => "blah" }
    p.assertNoAllocation("a1 4", "x")
  }

}
