// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package allocation
package parser

import a22o._, A22o._

class NumericSuite extends AllocationSuite {

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
      (int.token | expr.parens.token).merge

    expr.assertNoAllocation0("((1+1*2)+(3*4*5))/3") // failure case?

  }


}
