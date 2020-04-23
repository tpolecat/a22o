package a22o
package parser

import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class IntegerSuite extends ScalaCheckSuite with Numeric {

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
         .withMinSuccessfulTests(1000)

  property("parse any int") {
    forAll { (n: Int) =>
      assertEquals(int.parse(n.toString), (("", Right(n))))
    }
  }

  property("parse any int, with suffix") {
    forAll { (n: Int) =>
      val suffix = "abc"
      assertEquals(int.parse(n.toString + suffix), ((suffix, Right(n))))
    }
  }

  property("over/underflow") {
    forAll { (n: Long) =>
      val z = int.parse(n.toString)
      z match {
        case (_, Right(m)) => assert(n == m)
        case (_, Left(s))  =>
          assert(n < Int.MinValue.toLong || n > Int.MaxValue.toLong)
          assert(s == "Parse error at position 0: Integer over/underflow.")
      }
    }
  }

  test("empty string") {
    assertEquals(int.parse("")._2, Left("Parse error at position 0: Expected sign or digit."))
  }

  test("just a sign") {
    assertEquals(int.parse("+")._2, Left("Parse error at position 0: Expected digit."))
    assertEquals(int.parse("-")._2, Left("Parse error at position 0: Expected digit."))
    assertEquals(int.parse("+suffix")._2, Left("Parse error at position 0: Expected digit."))
    assertEquals(int.parse("-suffix")._2, Left("Parse error at position 0: Expected digit."))
  }

  test("boundary cases") {
    assertEquals(int.parse(Int.MinValue.toString)._2, Right(Int.MinValue))
    assertEquals(int.parse(Int.MaxValue.toString)._2, Right(Int.MaxValue))
    assert(int.parse((Int.MinValue.toLong - 1).toString)._2.isLeft)
    assert(int.parse((Int.MaxValue.toLong + 1).toString)._2.isLeft)
  }

}