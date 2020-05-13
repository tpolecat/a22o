
import a22o._, A22o._

// Ok so what can we say about invariants?
// - for parsers that consume nothing
//   - .consumed = const(0)
// - in all cases
//   - .void   is idempotent
//   - .consumed is idempotent
//   - .text   is idempotent
//   - .void.consumed.text.consumed ... .x = .x

// consumes nothing, yields ()
unit
unit.void == unit
unit.consumed
unit.peek == unit

// consumes nothing, yields a constant
const(42)
const(42).void
const(42).consumed
const(42).peek

// consumes nothing, fails with message
fail("abc")
fail("abc").void
fail("abc").consumed
fail("abc").peek

// consumes nothing, yields remaining input (allocates a string)
remaining
remaining.void
remaining.consumed
remaining.peek

// consumes nothing, yields number of characters remaining
available
available.void
available.consumed
available.peek

// ensures at least n characters remain, fails otherwise
ensure(10)
ensure(10).void
ensure(10).consumed
ensure(10).peek

// consume and discards n characters, failing if there are not enough
skip(10)
skip(10).void
skip(10).consumed
skip(10).peek

// consume and return n characters, failing if there are not enough
take(10)
take(10).void
take(10).consumed
take(10).peek
take(10).as("hi")

take(10).filter(_.length > 20).parse("1qeuydteuytdqtuy")
take(10).filter(_.length > 20).void
take(10).filter(_.length > 20).void.void

// consume and yield whatever `pa` does, if possible (yields an Option)
take(10).opt
take(10).opt.void
take(10).opt.consumed
take(10).opt.peek

// consume and yield whatever `pa` does, if possible (more general)
take(10).fold("")(_.toUpperCase())
take(10).fold("")(_.toUpperCase()).void
take(10).fold("")(_.toUpperCase()).consumed
take(10).fold("")(_.toUpperCase()).peek

// consume nothing and yield whatever `pa` does
take(10).peek
take(10).peek.void
take(10).peek.consumed
take(10).peek.peek

// same, with a different name
const(42).named("the-answer")
const(42).named("the-answer").void
const(42).named("the-answer").consumed

take(10).named("foo")
take(10).named("foo").void
take(10).named("foo").consumed

take(10).named("foo", Some("foo.void"))
take(10).named("foo", Some("foo.void")).void

// consumes one character meeting a predicate
accept(_.isLetter)
accept(_.isLetter).void
accept(_.isLetter).peek

// consumes any character
anyChar
anyChar.void
anyChar.peek

// consumes a specific character
char('c')
char('c').void
char('c').peek

// letters
letter
letter.void
letter.peek

// digits
digit
digit.void
digit.peek

// whitespace
whitespace
whitespace.void
whitespace.peek

// consumes one of several specific characters
charIn(List('p', 'e', 'a'))
charIn("banana")
charIn("banana").void
charIn("banana").peek

// discards input, yielding only the number of characters consumed
take(10).consumed
take(10).consumed.void


// THIS IS WRONG!
take(10).consumed.consumed

digit ~> letter