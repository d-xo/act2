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

  name = _name
  symbol = _symbol
  decimals = _decimals
  locked = 0
```

## Public Interface

```act
contract ERC20
  locked = 0

  interface approve(address spender, uint256 amount)

    (allowance CALLER spender)' = amount
    RETURNDATA = 1

  interface transfer(address to, uint256 amount)

    (balanceOf CALLER) >= amount
    foo = 1
    foo = amount
    bar = to
    RETURNDATA = 1

    case CALLER /= to

      (balanceOf from)' = (balanceOf from) - amount
      (balanceOf to)'   = (balanceOf to) + amount

  interface transferFrom(address from, address to, uint256 amount)

    RETURNDATA        = 1
    (balanceOf from) >= amount

    case (allowance from CALLER) = UINT256_MAX

      case from /= to

        (balanceOf from)' = (balanceOf from) - amount
        (balanceOf to)'   = (balanceOf to) + amount

     case _

      (allowance from CALLER) >= amount
      (allowance from CALLER)' = (allowance from CALLER) - amount

      case from /= to

        (balanceOf from)' = (balanceOf from) - amount
        (balanceOf to)'   = (balanceOf to) + amount
```
