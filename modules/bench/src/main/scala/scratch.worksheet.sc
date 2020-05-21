
import a22o._
import a22o.bench.Js.a22o._

number.parse("1")
number.parse("12")
number.parse("+12")
number.parse("-12")
number.parse("12.34")
number.parse("1.234e11")
number.parse("1.234e+11")
number.parse("1.234e-11")


unicodeEscape.parse("u1234")
unicodeEscape.parse("u124")
unicodeEscape.parse("u12x4")

escape.parse("""\bx""")
escape.parse("""\\x""")
escape.parse("""\/x""")
escape.parse("""\\u1234""")
escape.parse("""\/x""")
escape.parse("""\/x""")

jsonExpr.parse("""  "abc"  """)
jsonExpr.parse("""  123  """)
jsonExpr.parse("""  []  """)

string.parse(" \"foo\" ")

space.parse("  \n ")

number.parse("25")

Parser.takeWhile1(_.isDigit).parse("123")

Parser.stringOf1("0123456789").parse("123")

string.parse("   \"ab\\n\\uABCDc\"x")

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

jsonExpr.parse(example)

objBody.parse("\"foo\": 21")

string.many.sepBy(',').to(List).parse("\"foo\",\"bar\",\"baz\"")

pair.many.sepBy(',').to(List).parse("\"foo\":1,\"bar\":2,\"baz\":3")


('[' ~> pair.many.sepBy(',').to(List) <~ ']').parse("[\"foo\":1,\"bar\":2,\"baz\":3]")
obj.parse("{\"foo\":1,\"bar\":2,\"baz\":3}")

jsonExpr.parse("{\"abc\":42}")

jsonExpr.parse("{\"abc\":41}")
jsonExpr.parse(example)

Parser.digit.many.foldLeft(List.empty[Char])((t, h) => h :: t).parse("12345")
Parser.digit.many.to(List).parse("12345")

strChars.parse("foo")

strBody.parse("foo\\nbar")

Parser.digit.many.max(2).min(1).void.parse("123abc")

trait A {
  def foo(a: Int): Int
}

object B extends A {
  def foo(a: Int) = 42
}
