# one argument
type
  Node* = concept n
    `==`(n, n) is bool


# two argument
type Monoid = concept x, y
  x + y is type(x)
  type(z(type(x))) is type(x)


# with `var`
type RNG* = concept var rng
  rng.randomUint32() is uint32
