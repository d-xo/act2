contract ERC20:

  -- storage layout --

  storage:
    pub name        : str
    pub symbol      : str
    pub totalSupply : u256
    pub balanceOf   : addr -> u256
    pub allowance   : (addr, addr) -> u256

  -- invariants --

  inv @coq total_sum_balances
    = ∀ (a : addr) . sum([balanceOf(a)]) = [totalSupply]

  rel @smt no_inflation
    = ∀ (a : addr) . [totalSupply] = [totalSupply]!

  -- initializiation --

  constructor(_name : str, _symbol : str, supply : u256):
    rewrites:
      [name]! <- _name
      [symbol]! <- _symbol
      [totalSupply]! <- supply
      [balanceOf(CALLER)]! <- supply

  -- mutators --

  fn approve(usr : addr, amt : u256) -> bool:
    rewrites [allowance(CALLER,addr)]! <- amt
    returns true

  fn transfer(to : addr, amt : u256) -> bool:
    case (CALLER /= to):
      requires in u256:
        [balanceOf(CALLER)] - amt
        [balanceOf(to)] + amt
      rewrites move(CALLER, to, amt)
      returns true

    otherwise:
      requires in u256 ([balanceOf(CALLER)] - amt)
      returns true

  fn transferFrom(src : addr, dst : addr, amt : u256) -> bool:
    case (src /= dst && src /= CALLER):
      requires in u256:
        [balanceOf(src)] - amt
        [balanceOf(dst)] + amt
        [allowance(src, caller)] - amt

      rewrites:
        [allowance(src, CALLER)]! <- [allowance(src, CALLER)] - amt
        move(from, to, amount)

      returns true

    otherwise:
      requires in u256 ([balanceOf(src)] - amt)
      returns true

where
  move = macro!(src : addr, dst : addr, amt : u256) -> rewrites:
    [balanceOf(from)]! <- [balanceOf(from)] - amt
    [balanceOf(to)]!   <- [balanceOf(to)] + amt
