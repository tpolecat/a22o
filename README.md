# a22o

Like [**atto**](https://github.com/tpolecat/atto), but fast.

**a22o** takes some ideas from [this talk]() by Oscar Boykin and provides an API a lot like **atto** but with some ugly internals that make it fast.

- **a22o** allocates memory only for yielded results. This makes it pretty fast.
- Looping combinators such as `many` and `skipMany` have termination checks and fail parsing if input is not being consumed.

Note that (unlike its predecessor) **a22o** does not do incremental parsing.


## Primitive Constructors

- `const`
- `fail`

- `remaining`
- `available`
- `ensure`

- `take`
- `skip`

- `opt`



## Combinators

- `p.validate("n must be even")(_ % 2 == 0)` - unlike emap, doesn't allocate

