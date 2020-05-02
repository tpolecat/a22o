// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package allocation
package parser

import a22o.parser._
import cats.implicits._

class CombinatorSuite extends AllocationSuite with Combinator with Text {

  test("ok (none)") {
    ok(42).assertNoAllocation0("abc")
  }

  test("fail (none)") {
    fail[Int]("foo").assertNoAllocation1("abc")
  }

  test("ensure (none)") {
    ensure(3).assertNoAllocation("abcd", "ab")
  }

  test("ApBuilder mapN (components)") {
    val p = (digit ~ letter).mapN { (_, _) => "foo" }
    p.assertNoAllocation("1a", "x")
  }

  test("ApBuilder mapN (boxes)") {
    val p = (digit ~ letter).mapN { (n, c) => n + c }
    p.assertAllocation("1a", "x")(
      "java/lang/Integer" -> 1
    )
  }

  test("discardLeft (right component only)") {
    val p = discardLeft(take(1), letter)
    p.assertNoAllocation("abc", "x")
  }

  test("discardRight (left component only)") {
    val p = discardRight(letter, take(1))
    p.assertNoAllocation("abc", "x")
  }

  test("alt merge (components)") {
    val p = (letter | digit).merge
    p.assertNoAllocation("2", "_")
  }

  test("alt coproduct (components + Left/Right)") {
    val p = (letter | digit).coproduct
    p.assertAllocation("2", "_") {
      "scala/util/Right" -> 1
    }
  }

  test("many (builder + (components + ::) * N)") {
    val p = digit.many.as(List)
    p.assertAllocation0("12345")(
      "scala/collection/mutable/ListBuffer" -> 1,
      "scala/collection/immutable/$colon$colon" -> 10, // why??
      "scala/collection/StrictOptimizedLinearSeqOps$$anon$1" -> 1
    )
  }

  test("skipMany (none)") {
    val p = take(1).many.void
    p.assertNoAllocation0("12345")
  }

}
