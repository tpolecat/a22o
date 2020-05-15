// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package allocation
package parser

import a22o._, A22o._

class ParserSuite extends AllocationSuite {

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

}
