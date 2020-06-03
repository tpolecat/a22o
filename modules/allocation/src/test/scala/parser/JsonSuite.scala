// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package allocation
package parser

import a22o.Parser._

object Js {

  sealed trait Val extends Any {
    def value: Any
    def apply(i: Int): Val = this.asInstanceOf[Arr].value(i)
    def apply(s: java.lang.String): Val =
      this.asInstanceOf[Obj].value.find(_._1 == s).get._2
  }
  case class Str(value: java.lang.String) extends AnyVal with Val
  case class Obj(value: (java.lang.String, Val)*) extends AnyVal with Val
  case class Arr(value: Val*) extends AnyVal with Val
  case class Num(value: Double) extends AnyVal with Val
  case object False extends Val{
    def value = false
  }
  case object True extends Val{
    def value = true
  }
  case object Null extends Val{
    def value = null
  }

}

class JsonSuite extends AllocationSuite {

  def stringChars(c: Char) = c != '\"' && c != '\\'

  val space         = stringOf(" \r\n").void
  val digits        = stringOf1("0123456789").void
  val exponent      = (charIn("eE") ~ charIn("+-").orNot ~ digits).void
  val fractional    = ('.' ~ digits).void
  val integral      = ('0' | charIn("123456789")).void ~> digits.fold(())(_ => ())

  val number = (charIn("+-").void.fold(())(_ => ()) ~ integral ~ fractional.fold(())(_ => ()) ~ exponent.fold(())(_ => ())).inputText.map(
    x => Js.Num(x.toDouble)
  )

  val `null`        = "null".as(Js.Null)
  val `false`       = "false".as(Js.False)
  val `true`        = "true".as(Js.True)

  val hexDigit      = charIn("0123456789abcdefABCDEF")
  val unicodeEscape = ('u' ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit).void
  val escape        = ('\\' ~ (charIn("\"/\\bfnrt") | unicodeEscape).void).void

  val strChars = takeWhile1(stringChars)
  val string =
    (space ~ '"' ~ (strChars | escape).void.many.inputText ~ '"').mapN((_, _, s, _) => Js.Str(s))

  val array = '[' ~> jsonExpr.many.sepBy(','.token).to(List).map(Js.Arr(_:_*)) <~ ']'
    // P( "[" ~/ jsonExpr.rep(sep=","./) ~ space ~ "]").map(Js.Arr(_:_*))

  val pair = (string.map(_.value) ~ ':'.token ~ jsonExpr).mapN((s, _, v) => (s, v))

  val obj = '{' ~> pair.many.sepBy(','.token).to(List).map(Js.Obj(_:_*)) <~ '}'
  //   P( "{" ~/ pair.rep(sep=","./) ~ space ~ "}").map(Js.Obj(_:_*))

  val jsonExpr: Parser[Js.Val] =
    space ~> (
      obj |
      //  array |
        string |
      //  `true` |
      //  `false` |
      //  `null` |
        number
      ).merge <~ space

  test("string") {
    string.assertAllocation("  \"foo\"  ", "x")(
      "java/lang/String" -> 1,
      "char" -> 1,
      "a22o/allocation/parser/Js$Str" -> 1
    )
  }

  // test("string-1") {
  //   string.assertAllocation0(" \"foo\" ")(
  //     "java/lang/String" -> 1,
  //     "char" -> 1,
  //     "a22o/allocation/parser/Js$Str" -> 1
  //   )
  // }

  // test("number") {
  //   number.assertAllocation0NoVoid("0")(
  //     // "java/lang/String" -> 1,
  //     // "char" -> 1,
  //     // "a22o/allocation/parser/Js$Str" -> 1
  //   )
  // }

  // val example = """
  //   {
  //     "firstName": "John",
  //     "lastName": "Smith",
  //     "age": 25,
  //     "address": {
  //       "streetAddress": "21 2nd Street",
  //       "city": "New York",
  //       "state": "NY",
  //       "postalCode": 10021
  //     }
  //   }
  // """

  // test("example") {
  //   jsonExpr.assertAllocation0NoVoid(example)(
  //     "scala/collection/mutable/ListBuffer" -> 2,
  //     "sun/misc/FloatingDecimal$ASCIIToBinaryBuffer" -> 2,
  //     "a22o/allocation/parser/Js$Obj" -> 2,
  //     "java/lang/String" -> 15,
  //     "char" -> 17,
  //     "a22o/allocation/parser/Js$Num" -> 2,
  //     "a22o/allocation/parser/Js$Str" -> 13,
  //     "scala/collection/immutable/$colon$colon" -> 31,
  //     "scala/Tuple2" -> 8,
  //     "scala/collection/StrictOptimizedLinearSeqOps$$anon$1" -> 2
  //   )
  // }


}