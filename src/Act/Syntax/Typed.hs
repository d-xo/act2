{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Act.Syntax.Typed where

import Data.Text (Text)
import Data.Sequence (Seq)
import Data.HashMap (Map)
import Data.HashSet (Set)
import Data.HashSet qualified as Set
import Data.Parameterized.Some (Some(..))
import Data.Parameterized.List (List(..))
import Data.Parameterized.Classes
import Data.Parameterized.TH.GADT
import Generics.Kind.Derive.Eq
import Generics.Kind.Derive.FunctorOne


--- Contracts --------------------------------------------------------------------------------------


-- | Top level type representing a full specification of a contract
data Contract = Contract
  { name :: Text
  , storage :: StorageLayout
  , invariants :: Set Invariant
  , constructor :: Constructor
  , methods :: Set Method
  }


--- Behaviours -------------------------------------------------------------------------------------


-- | Initcode spec
data Constructor = Constructor
  { params :: Seq (Some (Binder Unquantified))
  , branches :: Cases CBranch
  }

-- | A description of a single branch of a constructor execution
data CBranch = CBranch
  { requires :: Set (Expr Unquantified Timed ABool)
  , rewrites :: Set (Some (Update Unquantified))
  , ensures :: Set (Expr Unquantified Timed ABool)
  }

-- | Method spec
data Method where
  Method ::
    { name :: Text
    , args :: Seq (Some (Binder Unquantified))
    , spec :: Cases (Branch a)
    , ret  :: STy a
    } -> Method

-- | A description of a single branch of a method execution
data Branch (a :: Ty) = Branch
  { requires :: Set (Expr Unquantified Timed ABool)
  , rewrites :: Set (Some (Update Unquantified))
  , returns :: Expr Unquantified Timed a
  , ensures :: Set (Expr Unquantified Timed ABool)
  }

-- | Potentially multiple execution branches
data Cases s
  = Cases (Map (Expr Unquantified Timed ABool) (Cases s)) s
  | Single s

-- | Introduce a new name
data Binder (q :: Quantity) (a :: Ty) where
  Binder ::
    { name :: Text
    , quantifier :: Quantifier q
    , ty :: STy a
    } -> Binder q a


--- Storage ----------------------------------------------------------------------------------------


-- | Layout of variables in storage
newtype StorageLayout = StorageLayout (Seq (Some Loc))

-- | Existential wrapper to make StorageLayout typecheck
data Loc (a :: Ty) where
  Loc :: Some (StorageLoc as) -> Loc a

-- | Rewrite loc into exp
data Update (q :: Quantity) (a :: Ty) where
  Rewrite ::
    { loc :: Loc a
    , exp :: Expr q Timed a
    } -> Update q a

-- | A pointer to a location in storage
data StorageLoc (as :: [Ty]) (a :: Ty) where
  StorageLoc ::
    { name :: Text
    , args :: List STy as
    , ret :: STy a
    } -> StorageLoc as a


-- | The result of reading an item from storage
data StorageItem (q :: Quantity) (t :: Timing) (a :: Ty) where
  StorageItem ::
    { time :: Time t
    , loc  :: StorageLoc as a
    , args :: List (Expr q t) as
    } -> StorageItem q t a

-- | Kind for Time expressions
data Timing
  -- ^ Fully explicit timing
  = Timed
  -- ^ Storage references can refer to either the pre or post state
  | Untimed

-- | Encodes choice between explicitly referring to the pre-/post-state, or not.
data Time t where
  Pre      :: Time Timed
  Post     :: Time Timed
  Whenever :: Time Untimed


--- Invariants -------------------------------------------------------------------------------------


-- | A contract level invariant
data Invariant = Invariant
  { name :: Text
  , prover :: Prover
  , vars :: Seq (Some (Binder Quantified))
  , prop :: Expr Quantified Untimed ABool
  }

-- | Supported invariant proof methods
data Prover
  = SMT
  | Coq

-- | Variables in invariants are quantified
data Quantity
  = Quantified
  | Unquantified

data Quantifier (q :: Quantity) where
  Forall :: Quantifier Quantified
  Exists :: Quantifier Quantified
  Neither :: Quantifier Unquantified

deriving instance Eq (Quantifier q)


--- Expressions ------------------------------------------------------------------------------------


-- | Kind for Expr types
data Ty
  = AInt
  | ABool
  | AStr

-- | Expressions
data Expr (q :: Quantity)(t :: Timing) (a :: Ty) where
  -- Variables and literals
  Lit :: STy a -> LitRep a -> Expr q t a
  Var :: STy ty -> Text -> Expr q t a
  Read :: StorageItem q t a -> Expr q t a

  -- Integer
  Add :: Expr q t AInt -> Expr q t AInt -> Expr q t AInt
  Sub :: Expr q t AInt -> Expr q t AInt -> Expr q t AInt
  Mul :: Expr q t AInt -> Expr q t AInt -> Expr q t AInt
  Div :: Expr q t AInt -> Expr q t AInt -> Expr q t AInt
  Mod :: Expr q t AInt -> Expr q t AInt -> Expr q t AInt
  Exp :: Expr q t AInt -> Expr q t AInt -> Expr q t AInt

  -- only available in quantified contexts (i.e. invariants)
  Sum :: StorageItem Quantified t a -> Expr Quantified t AInt

  -- Boolean
  And :: Expr q t ABool -> Expr q t ABool -> Expr q t ABool
  Or  :: Expr q t ABool -> Expr q t ABool -> Expr q t ABool
  Not :: Expr q t ABool -> Expr q t ABool -> Expr q t ABool
  LT  :: Expr q t AInt  -> Expr q t AInt  -> Expr q t ABool
  GT  :: Expr q t AInt  -> Expr q t AInt  -> Expr q t ABool
  GEq :: Expr q t AInt  -> Expr q t AInt  -> Expr q t ABool
  LEq :: Expr q t AInt  -> Expr q t AInt  -> Expr q t ABool
  Eq  :: STy a -> Expr q t a -> Expr q t a -> Expr q t ABool
  Imp :: Expr q t ABool -> Expr q t ABool -> Expr q t ABool

  -- constrains an integer expression to have a finite range
  -- needed because we encode this differently in different backends
  Fin ::
    { min :: LitRep AInt
    , max :: LitRep AInt
    , exp :: Expr q t AInt
    } -> Expr q t ABool

-- | The haskell type used to represent literals
type family LitRep (a :: Ty) where
  LitRep AInt = Integer
  LitRep ABool = Bool
  LitRep AStr = Text

-- | Singleton for Ty
data STy (a :: Ty) where
  SInt  :: STy AInt
  SBool :: STy ABool
  SStr  :: STy AStr


--- ERC20 ---


name, symbol :: StorageLoc '[] AStr
name = StorageLoc "name" Nil SStr
symbol = StorageLoc "symbol" Nil SStr

totalSupply :: StorageLoc '[] AInt
totalSupply = StorageLoc "totalSupply" Nil SInt

balanceOf :: StorageLoc '[AInt] AInt
balanceOf = StorageLoc "balanceOf" (SInt :< Nil) SInt

allowance :: StorageLoc '[AInt, AInt] AInt
allowance = StorageLoc "allowance" (SInt :< SInt :< Nil) SInt

storage :: StorageLayout
storage = StorageLayout
  [ Some (Loc (Some name))
  , Some (Loc (Some symbol))
  , Some (Loc (Some totalSupply))
  , Some (Loc (Some balanceOf))
  , Some (Loc (Some allowance))
  ]

invariant :: Invariant
invariant = Invariant
  { name = "total_sum_balances"
  , prover = Coq
  , vars = [Some (Binder "a" Forall SInt)]
  , prop = Eq SInt
      (Sum (StorageItem Whenever balanceOf (Var SInt "a" :< Nil)))
      (Read (StorageItem Whenever totalSupply Nil))
  }

constructor :: Constructor
constructor = Constructor
  { params = []
  , branches = Single $ CBranch
      { requires = Set.empty
      , rewrites = Set.empty
      , ensures = Set.empty
      }
  }

erc20 :: Contract
erc20 = Contract
  { name = "ERC20"
  , storage = storage
  , invariants = undefined
  , constructor = undefined
  , methods = undefined
  }

$(return [])

$(deriveGenericK ''Expr)

-- deriving instance Eq (Time t)
-- instance Eq (StorageItem 'Quantified 'Untimed a) where
--   (==) l@(StorageItem a b c) r@(StorageItem a1 b1 c1) = case testEquality l r of
--     Nothing -> False
--     Just Refl -> a == a1 && b == b1 && c == c1

instance TestEquality Time where
  testEquality = $(structuralTypeEquality [t|Time|] [])

instance TestEquality (Expr q t) where
  testEquality = $(structuralTypeEquality [t|Expr|]
    [ (ConType [t|STy|] `TypeApp` AnyType, [|testEquality|])
    , (ConType [t|StorageItem|] `TypeApp` AnyType `TypeApp` AnyType `TypeApp` AnyType, [|testEquality|])
    ])

instance TestEquality (StorageLoc args) where
  testEquality = $(structuralTypeEquality [t|StorageLoc|]
    [ (ConType [t|STy|] `TypeApp` AnyType, [|testEquality|])
    ])

instance TestEquality (StorageItem q t) where
  testEquality
    (StorageItem t0 (StorageLoc _ params0 ret0) args0)
    (StorageItem t1 (StorageLoc _ params1 ret1) args1)
      = do
          Refl <- testEquality t0 t1
          Refl <- testEquality ret0 ret1
          Refl <- testEquality params0 params1
          Refl <- testEquality args0 args1
          pure Refl

instance TestEquality STy where
  testEquality = $(structuralTypeEquality [t|STy|] [])

deriving instance Eq (STy a)


instance TestEquality (Binder q) where
  testEquality = $(structuralTypeEquality [t|Binder|]
    [ (ConType [t|STy|] `TypeApp` AnyType, [|testEquality|])
    ])

-- deriving instance (Eq (Expr Quantified Untimed ABool))
-- deriving instance (Eq (Expr Quantified Untimed AInt))
-- deriving instance (Eq Invariant)
-- deriving instance (Eq Prover)
