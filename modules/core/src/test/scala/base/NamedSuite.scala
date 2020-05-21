// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package base

class NamedSuite extends ParserSuite {

  property("p.named(...) <=> p") {
    assertEquivalent(p => (p, p.named("foo")))
  }

}
