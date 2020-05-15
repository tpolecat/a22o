import a22o._
import a22o.parser.all._

// An arithmetic expression parser that evaluates on the fly and doesn't allocate.

lazy val expr = sumand.many.sepBy(charIn("+-").token).foldSep(0)(identity) {
  case '+' => _ + _
  case '-' => _ - _
}

lazy val sumand = factor.many.sepBy(charIn("*/").token).foldSep(0)(identity) {
  case '*' => _ * _
  case '/' => _ / _
}

lazy val factor: Parser[Int] = (int.token | expr.parens.token).merge

expr.parse("6 + 9 * 8 / 2 * (735 + 1) + 6 + 3")




("foo" | 'b').foldN(_.length, _ => 42).parse("foob")

("foo" | 'b').coproduct.parse("foob")
