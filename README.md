# a22o

Like [**atto**](https://github.com/tpolecat/atto), but fast.

**a22o** takes some ideas from [this talk]() by Oscar Boykin and provides an API a lot like **atto** but with some ugly internals that make it fast.

**a22o** differs from **atto** in some important ways:

- It does not perform incremental parsing: `parser.parse("foo")` yields an immediate answer.
- Strictness issues that made it difficult to construct recursive parsers in **atto** have been fixed.
- **a22o** detects nontermination and fails parsing in some cases, such as `many(ok(42))`.




