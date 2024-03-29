```act
constructor of Token
interface constructor(string _symbol, string _name, string _version, uint _totalSupply)

creates

  string name         := _name
  string symbol       := _symbol
  uint256 totalSupply := _totalSupply
  mapping(address => uint) balanceOf :=  [CALLER := _totalSupply]
  mapping(address=>mapping(address=>uint)) allowance := []

invariants

  totalSupply == _totalSupply
```


```act
behaviour transfer of Token
interface transfer(uint256 value, address to)

iff

  CALLVALUE == 0
  value <= balanceOf[CALLER]
  CALLER =/= to => balanceOf[to] + value < 2^256

case CALLER =/= to:

  storage

     balanceOf[CALLER] => balanceOf[CALLER] - value
     balanceOf[to]     => balanceOf[to] + value

  ensures


     post(balanceOf[CALLER]) == pre(balanceOf[CALLER]) - value)

  returns 1

case CALLER == to:

  storage

    balanceOf[CALLER]
    balanceOf[to]

  returns 1
```

```act
behaviour transferFrom of Token
interface transferFrom(address src, address dst, uint amount)

iff

  amount <= balanceOf[CALLER]
  src    =/= dst => balanceOf[dst] + amount < 2^256
  CALLER =/= src => 0 <= allowance[src][CALLER] - amount
  CALLVALUE == 0

case src =/= dst and CALLER == src:

  storage

     balanceOf[CALLER]
     allowance[src][CALLER]
     balanceOf[src] => balanceOf[src] - amount
     balanceOf[dst] => balanceOf[dst] + amount

  returns 1

case src =/= dst and CALLER =/= src and allowance[src][CALLER] == 2^256 - 1:

  storage

     balanceOf[CALLER]
     allowance[src][CALLER]
     balanceOf[src] => balanceOf[src] - amount
     balanceOf[dst] => balanceOf[dst] + amount

  returns 1

case src =/= dst and CALLER =/= src and allowance[src][CALLER] < 2^256 - 1:

  storage

    balanceOf[CALLER]
    allowance[src][CALLER] => allowance[src][CALLER] - amount
    balanceOf[src]         => balanceOf[src] - amount
    balanceOf[dst]         => balanceOf[dst] + amount

  returns 1

case src == dst:

  storage

     balanceOf[CALLER]
     allowance[src][CALLER]
     balanceOf[src]
     balanceOf[dst]

  returns 1
```
