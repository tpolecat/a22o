// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o.bench

import org.openjdk.jmh.annotations._

object Trivial {

  object a22o {
    import _root_.a22o._
    import _root_.a22o.parser.all._

    @State(Scope.Benchmark)
    val letterOrDigit: Parser[Char] = letter | digit

  }

}


class Trivial {
  import Trivial._

  @Benchmark def a22o_letterOrDigit_letter = a22o.letterOrDigit.parse("a1")
  @Benchmark def a22o_letterOrDigit_digit  = a22o.letterOrDigit.parse("a1")
  @Benchmark def a22o_letterOrDigit_fail   = a22o.letterOrDigit.parse("*")

}