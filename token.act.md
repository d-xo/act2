# Act2

## Spec

```act
contract ERC20 {

  -- storage layout --

  storage {
    name        : str
    symbol      : str
    totalSupply : u256
    balanceOf   : addr -> u256
    allowance   : (addr, addr) -> u256
  }

  -- invariants --

  inv @coq total_sum_balances
    = ∀ (a : addr) . sum([balanceOf(a)]) = totalSupply

  -- initializiation --

  fn constructor(_name : str, _symbol : str, supply : u256) {
    rewrites {
      [name]' <- _name
      [symbol]' <- _symbol
      [totalSupply]' <- supply
      [balanceOf(CALLER)]' <- supply
    }
  }

  -- mutators --

  fn approve(usr : addr, amt : u256) -> bool {
    rewrites [allowance(CALLER,addr)]' <- amt
    returns true
  }

  fn transfer(to : addr, amt : u256) -> bool:
    case (CALLER /= to) {
      requires in u256
        [balanceOf(CALLER)] - amt
        [balanceOf(to)] + amt
      rewrites move(CALLER, to, amt)
      returns true
    }
    otherwise {
      requires in u256 ([balanceOf(CALLER)] - amt)
      returns true
    }
  }

  fn transferFrom(src : addr, dst : addr, amt : u256) -> bool {
    case (src /= dst && src /= CALLER) {
      requires in u256
        [balanceOf(src)] - amt
        [balanceOf(dst)] + amt
        [allowance(src, caller)] - amt

      rewrites
        [allowance(src, CALLER)]! <- [allowance(src, CALLER)] - amt
        move(from, to, amount)

      returns true
    }
    otherwise {
      requires in u256 ([balanceOf(src)] - amt)
      returns true
    }
  }

  -- state getters --

  fn allowance(src : addr, dst : addr) -> u256 {
    returns [allowance(src, dst)]
  }

  fn balanceOf(usr : addr) -> u256 {
    returns [balanceOf(usr)]
  }

  fn totalSupply() -> u256 {
    returns [totalSupply]
  }

  -- metadata --

  fn name() -> str {
    returns [name]
  }

  fn symbol() -> str {
    returns [symbol]
  }

  fn decimals() -> str {
    returns 18
  }
} where {
  move = macro!(src : addr, dst : addr, amt : u256) -> rewrites {
    [balanceOf(from)]! <- [balanceOf(from)] - amt
    [balanceOf(to)]!   <- [balanceOf(to)] + amt
  }
}
```

## AST

```haskell
data Ty
  = AInt
  | ABool
  | AStr

type family LitRep Ty where
  LitRep AInt = Integer
  LitRep ABool = Bool
  LitRrp AStr = Text

data STy (t :: Ty) where
  SInt  :: STy AInt
  SBool :: STy ABool
  SStr  :: STy AStr

data STime (t :: Time) where
  SPre :: STime Pre
  SPost :: STime Post

data Timing
  = Pre
  | Post

data Prover
  = SMT
  | Coq

data Quantifier
  = Forall
  | Exists

data Binder ty = Binder
  { name :: Text
  , ty :: STy ty
  }

data Invariant = Invariant
  { name :: Text
  , prover :: Prover
  , args :: Seq (Quantifier, Some Binder)
  , prop :: Prop
  }

data Cases t = Cases (Seq (Prop a)) a | Single a

data Constructor = Constructor
  { name :: Text
  , args :: Seq (Some Binder)
  , spec :: Cases CSpec
  }

data CSpec = CSpec
  { requires :: Seq Prop
  , rewrites :: Seq (Some Rewrite)
  , ensures :: Seq Prop
  }

data Method = Method
  { name :: Text
  , args :: Seq (Some Binder)
  , spec :: Cases Spec
  }

data Spec = Spec
  { requires :: Seq Prop
  , rewrites :: Seq (Some Rewrite)
  , ensures :: Seq Prop
  , returns :: Some Expr
  }

data Rewrite t = Rewrite
  { lhs = StorageItem Post t
  , rhs = Expr t
  }

data StorageItem (p :: Time) (ty :: Ty) where
  StorageItem :: STime p -> StorageLoc ty -> Seq (Some Expr) -> StorageItem p ty

data Prop where
  PEq :: Expr a -> Expr a -> Prop

data Expr (ty :: Ty) where
  Var :: STy ty -> Text -> Expr ty
  Lit :: STy ty -> LitRep ty -> Expr ty
  Add :: Expr AInt -> Expr AInt -> Expr AInt
  Sub :: Expr AInt -> Expr AInt -> Expr AInt
  Read :: StorageItem p t -> Expr t

newtype StorageLayout = StorageLayout (Seq (Some StorageLoc))

data StorageLoc t = StorageLoc
  { args :: Seq (Some Binder)
  , ret :: STy y
  }

data Contract = Contract
  { name :: Text
  , storage :: StorageLayout
  , invariants :: Seq Invariant
  , constructor :: Constructor
  , methods :: Seq Method
  }
```
