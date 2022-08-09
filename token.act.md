# ERC20 (Solmate)

https://github.com/transmissions11/solmate/blob/d155ee8d58f96426f57c015b34dee8a410c1eacc/src/tokens/ERC20.sol

## Storage Layout

```act
storage of ERC20

  locked      :: bool
  name        :: string
  symbol      :: string
  decimals    :: uint8
  totalSupply :: uint256
  balanceOf   :: address -> uint256
  allowance   :: address -> address -> uint256
```

## Constructor

```act
constructor of ERC20
interface constructor(string _name, string _symbol, uint8 _decimals)
let
  _name :: int -> int
  _symbol :: int -> int
  _decimals :: int
in
  in_range 8 _decimals
  name = _name
  symbol = _symbol
  decimals = _decimals
```

## Public Interface

```act
contract ERC20

  interface approve(address spender, uint256 amount)

    (allowance CALLER spender)' = amount
    RETURNDATA = 1

  interface transfer(address to, uint256 amount)

    in_range 256 ((balanceOf CALLER) - amount)
    in_range 256 ((balanceOf to) + amount)
    RETURNDATA = 1

    case CALLER /= to

      move CALLER to amount

  interface transferFrom(address from, address to, uint256 amount)

    in_range 256 ((balanceOf from) - amount)
    in_range 256 ((balanceOf to) + amount)
    RETURNDATA = 1

    case (allowance from CALLER) = UINT256_MAX

      case from /= to

        move from to amount

    case _

      (allowance from CALLER) >= amount
      (allowance from CALLER)' = (allowance from CALLER) - amount

      case from /= to

        move from to amount

where
  move :: address -> address -> int -> bool
  move src dst = and
    (balanceOf from)' = (balanceOf from) - amount
    (balanceOf to)'   = (balanceOf to) + amount
```
