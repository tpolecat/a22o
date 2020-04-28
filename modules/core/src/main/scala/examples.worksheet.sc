import a22o.{ ~, Parser }
import a22o.parser.all._
import cats.implicits._

anyChar.parse("")
anyChar.parse("foobar")
digit.parse("foo")
digit.parse("3oo")

pair(digit, letter).parse("3oo")
pair(digit, ???).parse("zoo")

digit.many.parse("bc")
digit.many.parse("1bc")
digit.many.parse("1826354873517865bc")


digit.many1.parse("bc")
digit.many1.parse("1bc")
digit.many1.parse("1826354873517865bc")


(digit || letter).parse("*")
(digit || letter).parse("*bc")
(digit || letter).parse("1bc")
(digit || letter).parse("abc")

val z = for {
  e <- digit || letter
  d <- digit
} yield (e, d)

z.parse("")
z.parse("*")
z.parse("*bc")
z.parse("1bc")
z.parse("x1c")
z.parse("22c")

(digit | ok("blah")).parse("x2") // Any


val m = (digit ~ digit ~ digit).map { case a ~ b ~ c => (a, b, c) }
m.parse("35")

((digit <~ letter) ~ digit).parse("1a2")
((digit ~> letter) ~ digit).parse("1a2")

(token(digit) ~ digit).parse("1 2x")

parens(digit).parse("(1)abc")
parens(digit).parse("(  1 )abc")

int.parse("")
int.parse("x")
int.parse("0")
int.parse("13xx")
int.parse("164839768765876576")
int.parse(Int.MaxValue.toString)
int.parse(Int.MinValue.toString)
int.parse((Int.MaxValue.toLong + 1L).toString)
int.parse((Int.MinValue.toLong - 1L).toString)

// TODO: sepBy, sepBy1
lazy val expr:   Parser[Int] = (sumand ~ char('+').token ~ sumand).map { case a ~ _ ~ b => a + b } | sumand
lazy val sumand: Parser[Int] = (factor ~ char('*').token ~ factor).map { case a ~ _ ~ b => a * b } | factor
lazy val factor: Parser[Int] = int.token | parens(expr).token

expr.parse("(9 * (8 * (7 + 1))) + 3")

(int.token |+| int).parse("42 44")

int.parse("+")
int.parse("-")
int.parse("+suffix")
int.parse("-suffix")

many(ok("hi")).parse("foo")
many(letter).parse("foo")
many1(ok("hi")).parse("foo")
many1(letter).parse("foo")
skipMany(letter).parse("foo123")
skipMany(ok("hi")).parse("foo")

fail("foo").parse("")

take(3).parse("abcdef")
take(6).parse("abcdef")
take(7).parse("abcdef")
take(-1).parse("abcdef")

string("foo").parse("foobar")
string("fox").parse("foobar")
(digit ~> string("foo")).parse("3foobar")

takeWhile(_.isDigit).parse("abc")
takeWhile(_.isDigit).parse("765abc")
takeWhile1(_.isDigit).parse("abc")
takeWhile1(_.isDigit).parse("765abc")

(skip(3) ~> int).parse("abc123")

charIn("61&#^%Efwjmb").parse("%")

sepBy(digit, char(',')).parse("")
sepBy(digit, char(',')).parse(",")
sepBy(digit, char(',')).parse("1,")
sepBy(digit, char(',')).parse("1,2,3,4")



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
val integral = string("0") | charIn('1' to '9').map(_.toString) + stringOf('1' to '9')

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





