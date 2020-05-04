import a22o._, a22o.parser.all._

// https://github.com/lihaoyi/fastparse/blob/master/perftests/bench1/src/perftests/JsonParse.scala
object Js {
  sealed trait Val extends Any {
    def value: Any
    def apply(i: Int): Val = this.asInstanceOf[Arr].value(i)
    def apply(s: java.lang.String): Val =
      this.asInstanceOf[Obj].value.find(_._1 == s).get._2
  }
  // N.B. these are AnyVal in Haoyi's code
  case class Str(value: java.lang.String) extends Val
  case class Obj(value: (java.lang.String, Val)*) extends Val
  case class Arr(value: Val*) extends Val
  case class Num(value: Double) extends Val
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


// val space         = P( CharsWhileIn(" \r\n").? )
val space =  stringOf(" \r\n")

// val digits        = P( CharsWhileIn("0123456789"))
val digits = stringOf1("0123456789")

// val exponent      = P( CharIn("eE") ~ CharIn("+-").? ~ digits )
val exponent = (charIn("eE").map(_.toString) + charIn("+-").map(_.toString).opt.map(_.orEmpty) + digits)

// val fractional    = P( "." ~ digits )
val fractional = string(".") + digits

// val integral      = P( "0" | CharIn('1' to '9') ~ digits.? )
val integral = (string("0") | charIn('1' to '9').map(_.toString) + stringOf('1' to '9')).merge

integral.parse("0")
integral.parse("01")
integral.parse("123")

// val number = P( CharIn("+-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(
//   x => Js.Num(x.toDouble)
// )

val number = (charIn("+-").map(_.toString).opt.map(_.orEmpty) + integral + fractional.opt.map(_.orEmpty) + exponent.opt.map(_.orEmpty)).map {
  x => Js.Num(x.toDouble)
}

number.parse("123.45")
number.parse("123.45e-16")

// val `null`        = P( "null" ).map(_ => Js.Null)
val `null` = string("null").as(Js.Null)

// val `false`       = P( "false" ).map(_ => Js.False)
val `false` = string("false").as(Js.False)

// val `true`        = P( "true" ).map(_ => Js.True)
val `true` = string("true").as(Js.True)

// val hexDigit      = P( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
val hexDigit = charIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F')).map(_.toString)

// val unicodeEscape = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
val unicodeEscape = string("u") + hexDigit + hexDigit + hexDigit + hexDigit

// val escape        = P( "\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape) )

// val strChars = P( CharsWhile(StringChars) )
// val string =
//   P( space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Js.Str)

// val array =
//   P( "[" ~/ jsonExpr.rep(sep=",".~/) ~ space ~ "]").map(Js.Arr(_:_*))

// val pair = P( string.map(_.value) ~/ ":" ~/ jsonExpr )

// val obj =
//   P( "{" ~/ pair.rep(sep=",".~/) ~ space ~ "}").map(Js.Obj(_:_*))

// val jsonExpr: P[Js.Val] = P(
//   space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
// )

(stringOf("123") + stringOf("abc")).parse("112abcfoo")


val regex = "u[0-9a-fA-F]{4}".r
val pat = regex.pattern
val mat = pat.matcher("fooua6F9bar")
mat.find(3)
mat.start
mat.end
mat.group



