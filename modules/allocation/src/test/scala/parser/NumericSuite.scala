// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package allocation
package parser

import a22o.parser._

class NumericSuite extends AllocationSuite with Numeric {

  test("int (none)") {
    int.assertNoAllocation("123", "abc")
  }

}
