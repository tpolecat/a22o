// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package base

import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class FailSuite extends ScalaCheckSuite {
  import Parser.const

  property("fail always fails and consumes no input") {
    forAll { (s: String, m: String) =>
      val p = Parser.fail[Int](m)
      val r = p.parse(s).toRemainingAndEither
      assertEquals(r, (s, Left(m)))
    }
  }

  property("fail(a) == fail(a)") {
    forAll { (a: String) =>
      Parser.fail(a) == Parser.fail(a)
    }
  }

  property("fail(a).as(b) == fail(a)") {
    forAll { (a: String, b: String) =>
      Parser.fail(a).as(b) == Parser.fail(a)
    }
  }

  property("fail(a).consumed == fail(a)") {
    forAll { (s: String) =>
      Parser.fail(s).consumed == Parser.fail(s)
    }
  }

  property("fail(a).flatMap(f) == fail(a)") {
    forAll { (a: String, f: String => Int) =>
      def g(a: String) = const(f(a))
      Parser.fail(a).flatMap(g) == Parser.fail(a)
    }
  }

  property("fail(a).inputText == fail(a)") {
    forAll { (a: String) =>
      Parser.fail(a).inputText == Parser.fail(a)
    }
  }

  property("Parser.fail(a).map(f) == Parser.fail(a)") {
    forAll { (a: String, f: String => Int) =>
      Parser.fail(a).map(f) == Parser.fail(a)
    }
  }

  property("fail(a).onError(b) == const(b)") {
    forAll { (a: String, b: String) =>
      Parser.fail(a).onError(b) == const(b)
    }
  }

  property("fail(a).opt == const(None)") {
    forAll { (a: String) =>
      Parser.fail(a).opt == const(None)
    }
  }

  property("fail(a).peek == fail(a)") {
    forAll { (a: String) =>
      Parser.fail(a).peek == Parser.fail(a)
    }
  }

  property("fail(a).void == fail(a)") {
    forAll { (a: String) =>
      Parser.fail(a).void == Parser.fail(a)
    }
  }

}
