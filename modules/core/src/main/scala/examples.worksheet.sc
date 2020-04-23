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
