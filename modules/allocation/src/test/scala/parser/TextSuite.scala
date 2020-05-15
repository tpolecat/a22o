// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package allocation
package parser

import a22o._, A22o._

class TextSuite extends AllocationSuite {

  test("anyChar (none)") {
    anyChar.assertNoAllocation("abc", "")
  }

  test("accept (none)") {
    accept(_ == 'a').assertNoAllocation("abc", "def")
  }

  test("digit (none)") {
    digit.assertNoAllocation("123", "abc")
  }

  test("letter (none)") {
    letter.assertNoAllocation("abc", "123")
  }

  test("whitespace (none)") {
    whitespace.assertNoAllocation("\n", "x")
  }

  test("char (none)") {
    char('a').assertNoAllocation("abc", "123")
  }

  test("charIn (none)") {
    charIn("123abc").assertNoAllocation("abc", "xx")
  }

  test("token (same as parser argument)".ignore) {
    char('a').token.assertNoAllocation("a  bc", "x")
  }

  test("bracketed (same as parser argument)".ignore) {
    char('a').brackets.assertNoAllocation("[ a  ]bc", "x")
  }

  test("parens (same as parser argument)".ignore) {
    char('a').parens.assertNoAllocation("( a )bc", "x")
  }

  test("take (char[] + String)") {
    take(2).assertAllocation("foo", "x")(
      "char" -> 1,
      "java/lang/String" -> 1
    )
  }

  // void case doesn't succeed
  test("string (none)".ignore) {
    string("abc").assertNoAllocation("abcdef", "123")
  }

  test("stringOf (char[] + String)") {
    stringOf("abc").assertAllocation0("bbbbaaac86587")(
      "char" -> 1,
      "java/lang/String" -> 1
    )
  }

  test("stringOf1 (char[] + String)".ignore) {
    stringOf("abc").assertAllocation("bbbbaaac86587", "x")(
      "char" -> 1,
      "java/lang/String" -> 1
    )
  }

  test("takeWhile (char[] + String)") {
    takeWhile(_.isDigit).assertAllocation0("123abc")(
      "char" -> 1,
      "java/lang/String" -> 1
    )
  }

  test("takeWhile1 (char[] + String)".ignore) {
    takeWhile(_.isDigit).assertAllocation("123abc", "abc")(
      "char" -> 1,
      "java/lang/String" -> 1
    )
  }

  test("concat (char[] + String)") {
    val p = take(1) + take(2) + take(1) + take(1)
    p.assertAllocation("123abcdef", "abc")(
      "char" -> 1,
      "java/lang/String" -> 1
    )
  }

}
