// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o.bench

import org.openjdk.jmh.annotations._
import a22o.Parser

object Js {

  sealed trait Val extends Any {
    def value: Any
    def apply(i: Int): Val = this.asInstanceOf[Arr].value(i)
    def apply(s: java.lang.String): Val =
      this.asInstanceOf[Obj].value.find(_._1 == s).get._2
  }
  case class Str(value: java.lang.String) extends Val // AnyVal with Val
  case class Obj(value: Seq[(java.lang.String, Val)]) extends Val // AnyVal with Val
  case class Arr(value: Val*) extends Val // AnyVal with Val
  case class Num(value: Double) extends Val // AnyVal with Val
  case object False extends Val{
    def value = false
  }
  case object True extends Val{
    def value = true
  }
  case object Null extends Val{
    def value = null
  }

  object fastparse {
    import _root_.fastparse._, NoWhitespace._

    def stringChars(c: Char) = c != '\"' && c != '\\'

    def space[_: P]         = P( CharsWhileIn(" \r\n", 0) )
    def digits[_: P]        = P( CharsWhileIn("0-9") )
    def exponent[_: P]      = P( CharIn("eE") ~ CharIn("+\\-").? ~ digits )
    def fractional[_: P]    = P( "." ~ digits )
    def integral[_: P]      = P( "0" | CharIn("1-9")  ~ digits.? )

    def number[_: P] = P(  CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(
      x => Js.Num(x.toDouble)
    )

    def `null`[_: P]        = P( "null" ).map(_ => Js.Null)
    def `false`[_: P]       = P( "false" ).map(_ => Js.False)
    def `true`[_: P]        = P( "true" ).map(_ => Js.True)

    def hexDigit[_: P]      = P( CharIn("0-9a-fA-F") )
    def unicodeEscape[_: P] = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
    def escape[_: P]        = P( "\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape) )

    def strChars[_: P] = P( CharsWhile(stringChars) )
    def strBody1[_: P] = P( (strChars | escape) )
    def strBody[_: P] = P(strBody1.rep.!)
    def string[_: P] =
      P( space ~ "\"" ~/ strBody ~ "\"").map(Js.Str)

    def array[_: P] =
      P( "[" ~/ jsonExpr.rep(sep=","./) ~ space ~ "]").map(Js.Arr(_:_*))

    def pair[_: P] = P( string.map(_.value) ~/ ":" ~/ jsonExpr )

    def objBody[_: P] = pair.rep(sep=","./)

    def obj[_: P] =
      P( "{" ~/ objBody ~ space ~ "}").map(Js.Obj(_))

    def jsonExpr[_: P]: P[Js.Val] = P(
      space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
    )

  }

  object a22o {
    import _root_.a22o._, Parser._

    def stringChars(c: Char) = c != '\"' && c != '\\'

      val space         = stringOf(" \r\n").void
      val digits        = stringOf1("0123456789").void
      val exponent      = (charIn("eE") ~ charIn("+-").orNot ~ digits).void
      val fractional    = ('.' ~ digits).void
      val integral      = ('0' | charIn("123456789")).void ~> digits.orNot

      val number = (charIn("+-").orNot ~ integral ~ fractional.orNot ~ exponent.orNot).inputText.map(
        x => Js.Num(x.toDouble)
      )

      val `null`        = "null".as(Js.Null)
      val `false`       = "false".as(Js.False)
      val `true`        = "true".as(Js.True)

      val hexDigit      = charIn("0123456789abcdefABCDEF")
      val unicodeEscape = ('u' ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit).void
      val escape        = ('\\' ~ (charIn("\"/\\bfnrt") | unicodeEscape).void).void

      val strChars = takeWhile1(stringChars)
      val strBody1 = (strChars | escape).void
      val strBody = strBody1.many.inputText
      val string =  (space ~ '"' ~ strBody ~ '"').mapN((_, _, s, _) => Js.Str(s))

      val array = '[' ~> jsonExpr.many.sepBy(','.token).to(List).map(Js.Arr(_:_*)) <~ ']'
      // P( "[" ~/ jsonExpr.rep(sep=","./) ~ space ~ "]").map(Js.Arr(_:_*))

      val pair = (string.map(_.value) ~ ':'.token ~ jsonExpr).mapN((s, _, v) => (s, v))

      val objBody = pair.many.sepBy(','.token).to(List).map(Js.Obj(_))
      // val objBody = pair.many.sepBy(','.token).foldLeft(List.empty[(String, Js.Val)])((ps, p) => p :: ps).map(Js.Obj(_))
      val obj = '{' ~> objBody <~ '}'
    //   P( "{" ~/ pair.rep(sep=","./) ~ space ~ "}").map(Js.Obj(_:_*))

      val jsonExpr: Parser[Js.Val] =
        space ~>
        (
          obj |
          //  array |
          string |
          //  `true` |
          //  `false` |
          //  `null` |
          number
          ).merge       <~ space




      // sys.error("???") : Unit

  }

  val example = """
    {
      "firstName": "John",
      "lastName": "Smith",
      "age": 25,
      "address": {
        "streetAddress": "21 2nd Street",
        "city": "New York",
        "state": "NY",
        "postalCode": 10021
      }
    }
  """

  val example2 = """
    {
      "firstName": "John",
      "lastName": "Smith",
      "age": 25,
      "address": {
        "streetAddress": "21 2nd Street",
        "city": "New York",
        "state": "NY",
        "postalCode": 10021
      },
      "phoneNumbers": [
        {
          "type": "home",
          "number": "212 555-1234"
        },
        {
          "type": "fax",
          "number": "646 555-4567"
        }
      ]
    }
  """



}

class Js {
  import Js._

  // @Benchmark def digits_fastParse = _root_.fastparse.parse("12345", fastparse.digits(_))
  // @Benchmark def digits_a22o = a22o.digits.parse("12345")

  // @Benchmark def exponent_fastParse = _root_.fastparse.parse("e12", fastparse.exponent(_))
  // @Benchmark def exponent_a22o = a22o.exponent.parse("e12")

  // @Benchmark def exponent_signed_fastParse = _root_.fastparse.parse("e-12", fastparse.exponent(_))
  // @Benchmark def exponent_signed_a22o = a22o.exponent.parse("e-12")

  // @Benchmark def number_fastParse = _root_.fastparse.parse("25", fastparse.number(_))
  // @Benchmark def number_a22o = a22o.number.parse("25")

  // @Benchmark def strChars_fastParse = _root_.fastparse.parse("abc123 blah", fastparse.strChars(_))
  // @Benchmark def strChars_a22o = a22o.strChars.parse("abc123 blah")

  // @Benchmark def space_fastParse = _root_.fastparse.parse("  \n ", fastparse.space(_))
  // @Benchmark def space_a22o = a22o.space.parse("  \n ")

  // @Benchmark def unicodeEscape_fastParse = _root_.fastparse.parse("u1222", fastparse.unicodeEscape(_))
  // @Benchmark def unicodeEscape_a22o = a22o.unicodeEscape.parse("u1222")

  // @Benchmark def strBody_noEsc_fastParse = _root_.fastparse.parse("abc123def", fastparse.strBody(_))
  // @Benchmark def strBody_noEsc_a22o = a22o.strBody.parse("abc123ndef")



  // @Benchmark def string_fastParse = _root_.fastparse.parse(" \"foo\" ", fastparse.string(_))
  // @Benchmark def string_a22o = a22o.string.parse(" \"foo\" ")

  // @Benchmark def strBody1_fastParse = _root_.fastparse.parse("foo", fastparse.strBody1(_))
  // @Benchmark def strBody1_a22o = a22o.strBody1.parse("foo")

  // @Benchmark def strChars_fastParse = _root_.fastparse.parse("foo", fastparse.strChars(_))
  // @Benchmark def strChars_a22o = a22o.strChars.parse("foo")

  // @Benchmark def strChars_fail_fastParse = _root_.fastparse.parse("\\", fastparse.strChars(_))
  // @Benchmark def strChars_fail_a22o = a22o.strChars.parse("\\")

  // @Benchmark def escape_fastParse = _root_.fastparse.parse("\\n", fastparse.escape(_))
  // @Benchmark def escape_a22o = a22o.escape.parse("\\n")

  // @Benchmark def escape_fail_fastParse = _root_.fastparse.parse("x", fastparse.escape(_))
  // @Benchmark def escape_fail_a22o = a22o.escape.parse("x")

  // @Benchmark def strBody_fastParse = _root_.fastparse.parse("foo", fastparse.strBody(_))
  @Benchmark def strBody_a22o_parse  = a22o.strBody.parse("foo")

  // @Benchmark def objBody_fastParse = _root_.fastparse.parse("\"foo\": 21", fastparse.objBody(_))
  // @Benchmark def objBody_a22o = a22o.objBody.parse("\"foo\": 21")

  // @Benchmark def jsonExpr_fastParse = _root_.fastparse.parse(example, fastparse.jsonExpr(_))
  // @Benchmark def jsonExpr_a22o = a22o.jsonExpr.parse(example)

}
