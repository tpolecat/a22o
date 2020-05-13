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
