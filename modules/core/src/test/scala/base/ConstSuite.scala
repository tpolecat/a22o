// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package base

import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class ConstSuite extends ScalaCheckSuite {
  import Parser.{ const, unit }

  property("const always succeeds and consumes no input") {
    forAll { (s: String, n: Int) =>
      val p = const(n)
      val r = p.parse(s).toRemainingAndEither
      assertEquals(r, (s, Right(n)))
    }
  }

  property("const(()) == unit") {
    const(()) == unit
  }

  property("const(a) == const(a)") {
    forAll { (n: Int) =>
      const(n) == const(n)
    }
  }

  property("const(a).as(b) == const(b)") {
    forAll { (n: Int, s: String) =>
      const(n).as(s) == const(s)
    }
  }

  property("const(a).consumed == const(0)") {
    forAll { (s: String) =>
      const(s).consumed == const(0)
    }
  }

  // improve this
  property("const(a).flatMap(f) == f(a)") {
    forAll { (a: String) =>
      def f(a: String) = const(a.length)
      const(a).flatMap(f) == f(a)
    }
  }

  property("const(a).inputText == const(\"\")") {
    forAll { (a: String) =>
      const(a).inputText == const("")
    }
  }

  property("const(a).map(f) == const(f(a))") {
    forAll { (a: String, f: String => Int) =>
      const(a).map(f) == const(f(a))
    }
  }

  property("const(a).onError(b) == const(a)") {
    forAll { (a: String, b: String) =>
      const(a).onError(b) == const(a)
    }
  }

  property("const(a).opt == const(Some(a))") {
    forAll { (a: String) =>
      const(a).opt == const(Some(a))
    }
  }

  property("const(a).peek == const(a)") {
    forAll { (a: String) =>
      const(a).peek == const(a)
    }
  }

  property("const(a).void == unit") {
    forAll { (a: String) =>
      const(a).void == unit
    }
  }

}
