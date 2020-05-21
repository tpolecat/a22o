import a22o._
import a22o.parser.all._

anyChar.parse("")
anyChar.parse("foobar")
digit.parse("foo")
digit.parse("3oo")

(digit ~ letter).tupled.parse("3oo")
(digit ~ ???).tupled.parse("zoo")

// digit.many.to(List).parse("bc")
// digit.many.to(List).parse("1bc")
// digit.many.to(List).parse("1826354873517865bc")

// digit.many1.to(List).parse("bc")
// digit.many1.to(List).parse("1bc")
// digit.many1.to(List).parse("1826354873517865bc")

(digit | letter).either.parse("*")
(digit | letter).either.parse("*bc")
(digit | letter).either.parse("1bc")
(digit | letter).either.parse("abc")

val z = for {
  e <- (digit | letter).either
  d <- digit
} yield (e, d)

z.parse("")
z.parse("*")
z.parse("*bc")
z.parse("1bc")
z.parse("x1c")
z.parse("22c")

(digit | const("blah")).merge.parse("x2") // Any


val m = (digit ~ digit ~ digit).tupled
m.parse("35")

((digit <~ letter) ~ digit).tupled.parse("1a2")
((digit ~> letter) ~ digit).tupled.parse("1a2")

(digit.token ~ digit).tupled.parse("1 2x")

digit.parens.parse("(1)abc")
digit.parens.parse("(  1 )abc")

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
lazy val expr:   Parser[Int] = ((sumand ~ char('+').token ~ sumand).mapN { (a, _, b) => a + b } | sumand).merge
lazy val sumand: Parser[Int] = ((factor ~ char('*').token ~ factor).mapN { (a, _, b) => a * b } | factor).merge
lazy val factor: Parser[Int] = (int.token | expr.parens.token).merge

expr.parse("(9 * (8 * (7 + 1))) + 3")

(int.token ~ int).inputText.parse("42 44")

int.parse("+")
int.parse("-")
int.parse("+suffix")
int.parse("-suffix")

// // many(ok("hi")).parse("foo")
// // many(letter).parse("foo")
// // many1(ok("hi")).parse("foo")
// // many1(letter).parse("foo")
// // skipMany(letter).parse("foo123")
// // skipMany(ok("hi")).parse("foo")

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

// digit.many.sepBy(char(',')).to(List).parse("")
// digit.many.sepBy(char(',')).to(List).parse(",")
// digit.many.sepBy(char(',')).to(List).parse("1,")
// digit.many.sepBy(char(',')).to(List).parse("1,2,3,4")
