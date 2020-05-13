package a22o
package base

import org.scalacheck.Prop._
import a22o.arb._

class PeekSuite extends ParserSuite {

  property("p.peek <~ string(s) sameParserAs p") {
    forAll(parserWithInput) { case (p, s) =>
      val pʹ = p.peek <~ Parser.string(s)
      assertEquals(pʹ.parseAll(s), p.parseAll(s))
    }
  }

}
