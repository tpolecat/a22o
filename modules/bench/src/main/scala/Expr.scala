// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o.bench

import org.openjdk.jmh.annotations._

// jmh:run -i 5 -wi 5 -f1 -t1 .*Expr*.

// Benchmark             Mode  Cnt        Score       Error  Units
// Expr.a22o_expr       thrpt    5  1436408.102 ± 15435.276  ops/s
// Expr.fastparse_expr  thrpt    5   568360.093 ±  5866.981  ops/s

object Expr {

  object fastparse {
    import _root_.fastparse._, NoWhitespace._

    def number[_: P]: P[Int] = P( CharIn("0-9").rep(1).!.map(_.toInt) )
    def parens[_: P]: P[Int] = P( "(" ~/ addSub ~ ")" )
    def factor[_: P]: P[Int] = P( number | parens )

    def divMul[_: P]: P[Int] = P( factor ~ (CharIn("*/").! ~/ factor).rep ).map(eval)
    def addSub[_: P]: P[Int] = P( divMul ~ (CharIn("+\\-").! ~/ divMul).rep ).map(eval)
    def expr[_: P]: P[Int]   = P( addSub ~ End )

    def eval(tree: (Int, Seq[(String, Int)])) = {
      val (base, ops) = tree
      ops.foldLeft(base){ case (left, (op, right)) => op match{
        case "+" => left + right case "-" => left - right
        case "*" => left * right case "/" => left / right
      }}
    }

  }

  object a22o {
    import _root_.a22o._, Parser._

    lazy val number: Parser[Int] = int
    lazy val parens: Parser[Int] = '(' ~> addSub <~ ')'
    lazy val factor: Parser[Int] = (number | parens).merge

    lazy val divMul: Parser[Int] =
      factor.many.sepBy(charIn("*/")).foldSepA(0) {
        case '*' => _ * _
        case '/' => _ / _
      }

    lazy val addSub: Parser[Int] =
      divMul.many.sepBy(charIn("+-")).foldSepA(0) {
        case '+' => _ + _
        case '-' => _ - _
      }

    lazy val expr:   Parser[Int] = addSub <~ endOfInput

  }

}


class Expr {
  import Expr._

  @Benchmark def fastparse_expr = _root_.fastparse.parse("((1+1*2)+(3*4*5))/3", fastparse.expr(_))
  @Benchmark def a22o_expr = a22o.expr.parse("((1+1*2)+(3*4*5))/3")

}