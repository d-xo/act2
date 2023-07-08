{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Act.Syntax.Typed where

import Prelude hiding (LT, GT)

import Data.Text (Text)
import Data.Sequence (Seq(..))
import Data.HashMap (Map)
import Data.HashSet (Set)
import Data.HashSet qualified as Set
import Data.Parameterized.Some (Some(..))
import Data.Parameterized.Classes (ShowF, TestEquality(..))
import Data.Typeable


--- Contracts --------------------------------------------------------------------------------------


-- | Top level type representing a full specification of a contract
data Contract = Contract
  { name :: Text
  , storage :: StorageLayout
  , invariants :: Set Invariant
  , constructor :: Constructor
  , methods :: Set Method
  }
  deriving (Show, Eq)


--- Behaviours -------------------------------------------------------------------------------------


-- | Initcode spec
data Constructor = Constructor
  { params :: Seq (Some (Binder Unquantified))
  , branches :: Cases CBranch
  }
  deriving (Show, Eq)

-- | A description of a single branch of a constructor execution
data CBranch = CBranch
  { requires :: Set (Expr Unquantified Timed ABool)
  , rewrites :: Set (Some (Update Unquantified))
  , ensures :: Set (Expr Unquantified Timed ABool)
  }
  deriving (Show, Eq)

-- | Method spec
data Method where
  Method ::
    { name :: Text
    , args :: Seq (Some (Binder Unquantified))
    , spec :: Cases (Branch a)
    , ret  :: STy a
    } -> Method

deriving instance Show Method
instance Eq Method where
  (Method a b c d) == (Method a1 b1 c1 d1) = case testEquality d d1 of
    Nothing -> False
    Just Refl -> a == a1 && b == b1 && c == c1

-- | A description of a single branch of a method execution
data Branch (a :: Ty) = Branch
  { requires :: Set (Expr Unquantified Timed ABool)
  , rewrites :: Set (Some (Update Unquantified))
  , returns :: Expr Unquantified Timed a
  , ensures :: Set (Expr Unquantified Timed ABool)
  }
  deriving (Show, Eq)

-- | Potentially multiple execution branches
data Cases s
  = Cases (Map (Expr Unquantified Timed ABool) (Cases s)) s
  | Single s

deriving instance (Show s) => Show (Cases s)
deriving instance (Eq s) => Eq (Cases s)

-- | Introduce a new name
data Binder (q :: Quantity) (a :: Ty) where
  Binder ::
    { name :: Text
    , quantifier :: Quantifier q
    , ty :: STy a
    } -> Binder q a

deriving instance (ShowF (Binder q))
deriving instance (Show (Binder q t))
deriving instance (Eq (Binder q t))

instance TestEquality (Binder q) where
  testEquality (Binder _ _ r) (Binder _ _ r1) = testEquality r r1


--- Storage ----------------------------------------------------------------------------------------


-- | Layout of variables in storage
newtype StorageLayout = StorageLayout (Seq (Some StorageLoc))
  deriving (Eq, Show)

-- | Rewrite loc into exp
data Update (q :: Quantity) (a :: Ty) where
  Rewrite ::
    { loc :: StorageLoc a
    , exp :: Expr q Timed a
    } -> Update q a

deriving instance ShowF (Update q)
deriving instance Show (Update q t)
deriving instance Eq (Update q t)

instance TestEquality (Update q) where
  testEquality (Rewrite a _) (Rewrite a1 _) = testEquality a a1

-- | A pointer to a location in storage
data StorageLoc (a :: Ty) where
  StorageLoc ::
    { name :: Text
    , params :: Seq (Some STy)
    , ret :: STy a
    } -> StorageLoc a

deriving instance (ShowF StorageLoc)
deriving instance (Show (StorageLoc t))
deriving instance (Eq (StorageLoc t))

instance TestEquality StorageLoc where
  testEquality (StorageLoc _ _ r0) (StorageLoc _ _ r1) = testEquality r0 r1

-- | The result of reading an item from storage
data StorageItem (q :: Quantity) (t :: Timing) (a :: Ty) where
  StorageItem ::
    { time :: Time t
    , loc  :: StorageLoc a
    , args :: Seq (WExpr q t)
    } -> StorageItem q t a

deriving instance Eq (StorageItem q t a)
deriving instance Show (StorageItem q t a)

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

deriving instance (Show (Time t))
deriving instance (Eq (Time t))
instance TestEquality Time where
  testEquality Pre Pre = Just Refl
  testEquality Pre Post = Just Refl
  testEquality Post Pre = Just Refl
  testEquality Post Post = Just Refl
  testEquality Whenever Whenever = Just Refl
  testEquality _ _ = Nothing


--- Invariants -------------------------------------------------------------------------------------


-- | A contract level invariant
data Invariant = Invariant
  { name :: Text
  , prover :: Prover
  , vars :: Seq (Some (Binder Quantified))
  , prop :: Expr Quantified Untimed ABool
  }
  deriving (Show, Eq)

-- | Supported invariant proof methods
data Prover
  = SMT
  | Coq
  deriving (Show, Eq)

-- | Variables in invariants are quantified
data Quantity
  = Quantified
  | Unquantified
  deriving (Show, Eq, Typeable)

data Quantifier (q :: Quantity) where
  Forall :: Quantifier Quantified
  Exists :: Quantifier Quantified
  Neither :: Quantifier Unquantified

deriving instance Eq (Quantifier q)
deriving instance Show (Quantifier q)


--- Expressions ------------------------------------------------------------------------------------


-- | Kind for Expr types
data Ty
  = AInt
  | ABool
  | AStr
  deriving Typeable

-- | Existential wrapper so we can put this in lists
data WExpr (q :: Quantity)(t :: Timing) where
  WExpr :: STy a -> Expr q t a -> WExpr q t

deriving instance Show (WExpr q t)
instance Eq (WExpr q t) where
  (WExpr a b) == (WExpr a1 b1) = case testEquality a a1 of
    Just Refl -> b == b1
    Nothing -> False

-- | Expressions
data Expr (q :: Quantity)(t :: Timing) (a :: Ty) where
  -- Variables and literals
  Lit :: Val a -> Expr q t a
  Var :: STy a -> Text -> Expr q t a
  Read :: StorageItem q t a -> Expr q t a

  -- Integer
  Add :: Expr q t AInt -> Expr q t AInt -> Expr q t AInt
  Sub :: Expr q t AInt -> Expr q t AInt -> Expr q t AInt
  Mul :: Expr q t AInt -> Expr q t AInt -> Expr q t AInt
  Div :: Expr q t AInt -> Expr q t AInt -> Expr q t AInt
  Mod :: Expr q t AInt -> Expr q t AInt -> Expr q t AInt
  Exp :: Expr q t AInt -> Expr q t AInt -> Expr q t AInt

  -- only available in quantified contexts (i.e. invariants)
  Sum :: StorageItem Quantified t AInt -> Expr Quantified t AInt

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
    { min :: Integer
    , max :: Integer
    , exp :: Expr q t AInt
    } -> Expr q t ABool

deriving instance ShowF (Expr q t)
deriving instance Show (Expr q t a)
deriving instance Typeable (Expr q t a)

data Val (a :: Ty) where
  VInt :: Integer -> Val AInt
  VBool :: Bool -> Val ABool
  VStr :: Text -> Val ABool

deriving instance Show (Val a)
deriving instance Eq (Val a)
instance TestEquality Val where
  testEquality (VInt _) (VInt _) = Just Refl
  testEquality (VBool _) (VBool _) = Just Refl
  testEquality (VStr _) (VStr _) = Just Refl
  testEquality _ _ = Nothing

-- | Singleton for Ty
data STy (a :: Ty) where
  SInt  :: STy AInt
  SBool :: STy ABool
  SStr  :: STy AStr

deriving instance ShowF STy
deriving instance Show (STy t)
deriving instance Eq (STy a)

instance TestEquality STy where
  testEquality SInt SInt = Just Refl
  testEquality SBool SBool = Just Refl
  testEquality SStr SStr = Just Refl
  testEquality _ _ = Nothing

instance Eq (Expr q t a) where
  Lit a == Lit a1 = a == a1
  Var a b == Var a1 b1 = case testEquality a a1 of
    Just Refl -> b == b1
    Nothing -> False
  Read a == Read b = a == b
  Add a b == Add a1 b1 = a == a1 && b == b1
  Sub a b == Sub a1 b1 = a == a1 && b == b1
  Mul a b == Mul a1 b1 = a == a1 && b == b1
  Div a b == Div a1 b1 = a == a1 && b == b1
  Mod a b == Mod a1 b1 = a == a1 && b == b1
  Exp a b == Exp a1 b1 = a == a1 && b == b1
  Sum a == Sum a1 = a == a1

  And a b == And a1 b1 = a == a1 && b == b1
  Or a b == Or a1 b1 = a == a1 && b == b1
  Not a b == Not a1 b1 = a == a1 && b == b1
  LT a b == LT a1 b1 = a == a1 && b == b1
  GT a b == GT a1 b1 = a == a1 && b == b1
  GEq a b == GEq a1 b1 = a == a1 && b == b1
  LEq a b == LEq a1 b1 = a == a1 && b == b1
  Imp a b == Imp a1 b1 = a == a1 && b == b1
  Eq a b c == Eq a1 b1 c1 = case testEquality a a1 of
    Just Refl -> b == b1 && c == c1
    Nothing -> False
  Fin a b c == Fin a1 b1 c1 = a == a1 && b == b1 && c == c1
  _ == _ = False


--- ERC20 ---


name, symbol :: StorageLoc AStr
name = StorageLoc "name" mempty SStr
symbol = StorageLoc "symbol" mempty SStr

totalSupply :: StorageLoc AInt
totalSupply = StorageLoc "totalSupply" mempty SInt

balanceOf :: StorageLoc  AInt
balanceOf = StorageLoc "balanceOf" [Some SInt] SInt

allowance :: StorageLoc  AInt
allowance = StorageLoc "allowance" [Some SInt, Some SInt] SInt

storage :: StorageLayout
storage = StorageLayout
  [ Some name
  , Some symbol
  , Some totalSupply
  , Some balanceOf
  , Some allowance
  ]

invariant :: Invariant
invariant = Invariant
  { name = "total_sum_balances"
  , prover = Coq
  , vars = [Some (Binder "a" Forall SInt)]
  , prop = Eq SInt
      (Sum (StorageItem Whenever balanceOf [WExpr SInt (Var SInt "a")]))
      (Read (StorageItem Whenever totalSupply []))
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
