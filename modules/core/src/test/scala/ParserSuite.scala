// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o

import cats.implicits._
import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import a22o.arb._

trait ParserSuite extends ScalaCheckSuite {

  def assertEquivalent(f: Parser[Any] => (Parser[Any], Parser[Any])) =
    forAll(parserWithInput.map { case (p, s) => (f(p), s) }) { case ((p1, p2), s) =>
      assertEquals(p1.parse(s), p2.parse(s))
  }

  def assertEquivalentPredicate(f: Parser[Any] => (Parser[Any], Parser[Any])) =
    forAll(parserWithInput.map { case (p, s) => (f(p), s) }) { case ((p1, p2), s) =>
      assertEquals(p1.parse(s).map(_.isRight), p2.parse(s).map(_.isRight))
  }

}
