
import a22o._, A22o._


unit
unit.map(_ => "foo")
unit.flatMap(_ => string("blergh"))
unit.consumed

const(5).as("abc")
const(5).map(_ + 1)
const(5).void


fail("oh noes")
fail[Int]("oh noes").map(_ + 1)
fail("x").void
fail("x").onError("blah")

whitespace
whitespace.void

take(10).void


skip(0)
skip(1).parse("\u0000")