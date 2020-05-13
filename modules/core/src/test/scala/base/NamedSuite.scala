package a22o
package base

class NamedSuite extends ParserSuite {

  property("p.named(...) <=> p") {
    assertEquivalent(p => (p, p.named("foo")))
  }

}
