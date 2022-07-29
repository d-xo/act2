# Act2

## Core Language

### Specifications

A specification in act is a constraint system that evaluates to `true`.

### Types

There are two core types in act:

  - int  : an unbounded integer
  - bool : a boolean

We can introduce literals of the above types as follows:

  - int  : a number (e.g. `100`)
  - bool : `true`, `false`

Variables can be of any type (including predicates)

### Predicates

Users can define custom predicates using the `->` constructor.

For example:

`balanceOf :: int -> int -> int`

Is an uninterpreted predicate called `balanceOf` that accepts two `int`s and returns an `int`.

While the following are predicates with a given interpretation:

```act
foo :: int -> int -> bool
foo a b = and
  a < 100
  b = 1
```

```act
bar :: int -> int -> int
bar a b = a + (b * a)
```

### Scopes

The final type checked AST has no notion of scope.
The concrete syntax has a notion of local scope that exists only to catch errors introduced by
typos.

Variables can be introduced using `let A in B` which has type `bool`.

```act
let
  a :: Int
in
  a = b
```

Variables can only be referenced if they have been introduced via let beforehand.

### Typing Statements

TODO...

## Sugar

### `and` and `or`

`and` and `or` introduce a new scope.
Each line at a new indentation level following these operators is interpreted as an argument to that operator.

for example:

```act
and
  a = b
  or
    d < e
    a = e
  and
    d = f
    f > g
```

is equivalent to:

```act
(a = b) `and` (((d < e) `or` (a = e)) `and` ((d = f) `and` (f > g)))
```

### `String`

### `Interface`

### `Solidity Types`

## Built In Predicates

### Boolean Logic

- `and` : bool -> bool -> bool
- `or`  : bool -> bool -> bool
- `not` : bool -> bool

### Integers

- `>`  : int -> int -> bool
- `>=` : int -> int -> bool
- `<`  : int -> int -> bool
- `<=` : int -> int -> bool

- `+`  : int -> int -> int
- `-`  : int -> int -> int
- `*`  : int -> int -> int
- `/`  : int -> int -> int

### Polymorphic

In the following `a` is an atomic type (either `int` or `bool`).

- `=`   : a -> a -> bool
- `/=`  : a -> a -> bool

### Blockchain Context

- `CALLER`    : int
- `NUMBER`    : int
- `TIMESTAMP` : int

