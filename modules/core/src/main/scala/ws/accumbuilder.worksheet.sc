import a22o._, a22o.parser.all._

val p = int.token.many.foldLeft(0)(_ + _)
p.parse("")
p.parse("1")
p.parse("1 2")
p.parse("1 2 3")


lazy val expr: Parser[Int] =
  product.many1.sepBy(charIn("+-")).reduceSepA {
    case '+' => _ + _
    case '-' => _ - _
  }

lazy val product: Parser[Int] =
  factor.many1.sepBy(charIn("*/")).reduceSepA {
    case '*' => _ * _
    case '/' => _ / _
  }

lazy val factor: Parser[Int] =
  (int | parens(expr)).merge

expr.parse("((1+1*2)+(3*4*5))/3*2")
expr.parse("123")
expr.parse("xyz")


// ---

sealed trait Json
case class JInt(value: Int) extends Json
case class JArr(value: List[Json]) extends Json

lazy val json: Parser[Json] = (jint | jarr).merge
lazy val jint: Parser[JInt] = int.map(JInt)
lazy val jarr: Parser[JArr] = brackets(json.token.many.sepBy(char(',').token).as(List)).map(JArr)

json.parse("42")
json.parse("[42, 128, [1, [ 2 ], [ ] , 3]]")

