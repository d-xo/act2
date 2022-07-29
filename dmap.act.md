# Dmap

https://github.com/dapphub/dmap/tree/5a6055ad2d4201e9145cf9882bbd71c2f9d16add

Dmap is a permissioned key / value store. Each address on ethereum can associate data with names
within their own segregated namespace. Names can be optionally locked, making further changes to the
associated data impossible.

Each name has two associated values:

- `data`: the data associated with a name
- `meta`: a field for storing metadata about that name
  - the last bit is reserved as a flag indicating whether a name has been locked
  - the remaining 256 are available for name owners to use as they wish

## Storage Layout

```act
storage of Dmap

  LOCK :: uint256
  root :: address
  meta :: address -> bytes32 -> bytes32
  data :: address -> bytes32 -> bytes32
```

## Constructor

```act
constructor of Dmap
interface constructor(address rootzone)

  LOCK' = 1
  root' = rootzone
```

## Public Interface

```act
contract Dmap

  case CALLDATASIZE = 36

    // get(bytes32 name)
    name       = calldata 4
    RETURNDATA = (meta CALLER name) ++ (data CALLER name)

  case CALLDATASIZE = 100

    // set(bytes32 name, bytes32 m, bytes32 d)
    name = calldata 4
    m    = calldata 36
    d    = calldata 68

    unlocked (meta CALLER name)

    (meta CALLER name)' = m
    (data CALLER name)' = d
```

### Utils

```act
// a name is unlocked if the last bit in it's meta word is 0
unlocked :: bytes32 -> type
unlocked m = (m % 2 = 0)
```

