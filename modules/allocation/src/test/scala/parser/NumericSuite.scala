// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package allocation
package parser

import a22o._
import a22o.parser._
import a22o.parser.all._

class NumericSuite extends AllocationSuite with Numeric {

  test("int (none)") {
    int.assertNoAllocation("123", "abc")
  }

  test("interpreter!") {

    lazy val expr: Parser[Int] =
      product.many.min(1).sepBy(charIn("+-").token).foldSepA(0) {
        case '+' => _ + _
        case '-' => _ - _
      }

    lazy val product: Parser[Int] =
      factor.many.sepBy(charIn("*/").token).foldSepA(0) {
        case '*' => _ * _
        case '/' => _ / _
      }

    lazy val factor: Parser[Int] =
      (int.token | parens(expr).token).merge

    expr.assertNoAllocation0("((1+1*2)+(3*4*5))/3") // failure case?

  }


}
