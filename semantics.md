# Act2

An act spec is a description of the non reverting behaviour of an EVM program.

## Core Language

### Specifications

A specification in act is a constraint system that evaluates to `true`.

### Types

There are two core types in act:

  - int       : an unbounded integer
  - bool      : a boolean

We can introduce literals of the above types as follows:

  - int  : a number (e.g. `100`)
  - bool : `true`, `false`

There are two kinds of variables:

  - Arg : a reference to calldata or the environment
  - Ref : a reference to storage (either to the pre or post state)

Variables are always either an int or a bool.

```haskell
-- Core datatypes
data ActType
  = Int
  | Bool

data Timing
  = Pre
  | Post

data Act (a :: ActType) where
  -- Lits
  LitBool :: Bool -> Act Bool
  LitInt :: Int -> Act Int

  -- Vars
  Ref :: Timing -> String -> Act a
  Arg :: String -> Act a
  Env :: EthEnv -> Act a

  -- Ops
  And :: Act Bool -> Act Bool -> Act Bool
  Or :: Act Bool -> Act Bool -> Act Bool

-- Typing Statements
data Atom where
  IntDef :: Atom
  BoolDef :: Atom

data Composite where
  FunDef :: [Atom] -> Atom -> Composite

data TypeDef where
  A :: Atom -> TypeDef
  C :: Composite -> TypeDef

-- Top Level AST
newtype Scope = Scope [(String, TypeDef)]
newtype Constructor = Constructor Scope (Act Bool)
newtype Behaviour = Behaviour Scope (Act Bool)
data Contract = Contract Scope Constructor [Behaviour]
```

### Functions

Users can define functions using the `->` constructor.

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

Abstract variables can be introduced using `let A in B`, where B has type Bool.

```act
let
  a :: Int
in
  a = b
```

Variables can only be referenced if they have been introduced via let beforehand.

### Typing Statements

There are two atomic types (int and bool).
Function types can be introduced using the `->` constructor.

`-> :: Type -> Type -> Type` is a binary type operator that produces a function type.

map :: (int -> int) -> int -> int

Functions can only take

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

## Problems

The problem with representing state updates as constraints, is that some state variables may be left
in an underspecifed state.

```act
upd :: (int -> int -> int) -> int -> int -> (int -> int -> int) -> Prop
upd f a b prev = if a == 3 && b == 6
                   then f a b == 10
                 else if a == 5 && b == 7
                   then f a b == 4
                 else f a b == prev a b

storage of A

  x :: uint
  y :: uint -> uint

contract A

  interface foo()

    x > 0
```
