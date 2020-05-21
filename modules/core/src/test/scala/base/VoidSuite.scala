// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package base

class VoidSuite extends ParserSuite {

  property("p samePredicateAs p.void") {
    assertEquivalentPredicate(p => (p, p.void))
  }

  property("p.void.void sameParserAs p.void") {
    assertEquivalent(p => (p.void.void, p.void))
  }

}
