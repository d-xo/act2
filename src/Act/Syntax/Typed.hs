{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Act.Syntax.Typed where

import Prelude hiding (LT, GT)

import Data.Text (Text)
import Data.Sequence (Seq(..))
import Data.Hashable
import Data.HashMap (Map)
import Data.HashSet (Set)
import Data.HashSet qualified as Set
import Data.Parameterized.Some (Some(..))
import Data.Parameterized.Classes
import Data.Parameterized.TH.GADT
import Data.Typeable
import GHC.Generics hiding (Constructor)


-- * Contracts


-- | Top level type representing a full specification of a contract
data Contract = Contract
  { name :: Text
  , storage :: StorageLayout
  , invariants :: Set Invariant
  , relations :: Set Relation
  , constructor :: Constructor
  , methods :: Set Method
  }
  deriving (Show, Eq, Generic)


-- * Behaviours


-- | Initcode spec
data Constructor = Constructor
  { params :: Seq (Some (Binder Unquantified))
  , branches :: Cases CBranch
  }
  deriving (Show, Eq, Generic)

-- | A description of a single branch of a constructor execution
data CBranch = CBranch
  { requires :: Set (Expr Unquantified Timed ABool)
  , rewrites :: Set (Some (Update Unquantified))
  , ensures :: Set (Expr Unquantified Timed ABool)
  }
  deriving (Show, Eq, Generic)

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
  deriving (Show, Eq, Generic)

-- | Potentially multiple execution branches
data Cases s
  = Cases (Map (Expr Unquantified Timed ABool) (Cases s)) s
  | Single s
  deriving (Generic)

deriving instance (Show s) => Show (Cases s)
deriving instance (Eq s) => Eq (Cases s)

-- | Introduce a new name
data Binder (q :: Quantity) (a :: Ty) where
  Binder ::
    { quantifier :: Quantifier q
    , name :: Text
    , ty :: STy a
    } -> Binder q a

deriving instance (ShowF (Binder q))
deriving instance (Show (Binder q t))
deriving instance (Eq (Binder q t))
instance TestEquality (Binder q) where
  testEquality (Binder _ _ r) (Binder _ _ r1) = testEquality r r1


-- * Storage


-- | Layout of variables in storage
newtype StorageLayout = StorageLayout (Seq (Some StorageLoc))
  deriving newtype (Eq, Show)

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


-- * Invariants / Relations


-- | A contract level relation
data Relation = Relation
  { name :: Text
  , prover :: Prover
  , vars :: Seq (Some (Binder Quantified))
  , prop :: Expr Quantified Timed ABool
  }
  deriving (Show, Eq, Generic)

-- | A contract level invariant
data Invariant = Invariant
  { name :: Text
  , prover :: Prover
  , vars :: Seq (Some (Binder Quantified))
  , prop :: Expr Quantified Untimed ABool
  }
  deriving (Show, Eq, Generic)

-- | Supported invariant proof methods
data Prover
  = SMT
  | Coq
  deriving (Show, Eq, Generic, Hashable)

-- | Variables in invariants are quantified
data Quantity
  = Quantified
  | Unquantified
  deriving (Show, Eq, Generic, Hashable)

data Quantifier (q :: Quantity) where
  Forall :: Quantifier Quantified
  Exists :: Quantifier Quantified
  X      :: Quantifier Unquantified

deriving instance Eq (Quantifier q)
deriving instance Show (Quantifier q)
instance TestEquality Quantifier where
  testEquality Forall Forall = Just Refl
  testEquality Forall Exists = Just Refl
  testEquality Exists Forall = Just Refl
  testEquality X X = Just Refl
  testEquality _ _ = Nothing


-- * Expressions


{-|
  Symbolic Computations

  There are different restrictions on allowable expressions in different parts of the syntax tree. The type parameters allow us to make constructing instances that violate these restrictions a type error.

  - Quantity: Whether variables should be quantified or not
  - Timing: Whether storage references should refer explicitly to the pre/post state
  - Ty: The type of the value described by the expression

  This approach allows us to
-}
data Expr (q :: Quantity) (t :: Timing) (a :: Ty) where
  -- Variables and literals
  Lit :: Val a -> Expr q t a
  Var :: Quantifier q -> Text -> STy ty -> Expr q t a
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
instance Eq (Expr q t a) where
  Lit a == Lit a1 = a == a1
  Var a b c == Var a1 b1 c1 = case testEquality a a1 of
    Just Refl -> case testEquality c c1 of
      Just Refl -> b == b1
      Nothing -> False
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


-- ** Auxiliary Types


-- | Kind for Expr types
data Ty
  = AInt
  | ABool
  | AStr

-- | Value wrapper type to allow for a polymorphic Lit
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

-- | Existential wrapper so we can put (Expr q t) into lists
data WExpr (q :: Quantity)(t :: Timing) where
  WExpr :: STy a -> Expr q t a -> WExpr q t

deriving instance Show (WExpr q t)
instance Eq (WExpr q t) where
  (WExpr a b) == (WExpr a1 b1) = case testEquality a a1 of
    Just Refl -> b == b1
    Nothing -> False


-- * Hashable Instances


-- We have to do these instances all at once underneath this top level
-- splice because of the way that template haskell splices affect mutual
-- recursion within a file. see: https://blog.monadfix.com/th-groups
$(return[])

deriving instance Hashable Invariant
deriving instance Hashable Constructor
deriving instance Hashable CBranch
deriving instance Hashable Contract
deriving instance Hashable Relation
deriving newtype instance Hashable StorageLayout
deriving instance Hashable s => Hashable (Cases s)

instance Hashable (Branch a) where
  hashWithSalt = $(structuralHashWithSalt [t|Branch|] [])
instance Hashable Method where
  hashWithSalt = $(structuralHashWithSalt [t|Method|] [])
instance Hashable (STy a) where
  hashWithSalt = $(structuralHashWithSalt [t|STy|] [])
instance HashableF STy where
  hashWithSaltF = $(structuralHashWithSalt [t|STy|] [])
instance HashableF (Update q) where
  hashWithSaltF = $(structuralHashWithSalt [t|Update|] [])
instance Hashable (Quantifier q) where
  hashWithSalt = $(structuralHashWithSalt [t|Quantifier|] [])
instance HashableF (Binder q) where
  hashWithSaltF = $(structuralHashWithSalt [t|Binder|] [])
instance Hashable (Expr q t a) where
  hashWithSalt = $(structuralHashWithSalt [t|Expr|] [])
instance HashableF (Expr q t) where
  hashWithSaltF = $(structuralHashWithSalt [t|Expr|] [])
instance Hashable (StorageItem q t a) where
  hashWithSalt = $(structuralHashWithSalt [t|StorageItem|] [])
instance Hashable (StorageLoc a) where
  hashWithSalt = $(structuralHashWithSalt [t|StorageLoc|] [])
instance HashableF StorageLoc where
  hashWithSaltF = $(structuralHashWithSalt [t|StorageLoc|] [])
instance Hashable (WExpr q t) where
  hashWithSalt = $(structuralHashWithSalt [t|WExpr|] [])
instance Hashable (Val a) where
  hashWithSalt = $(structuralHashWithSalt [t|Val|] [])
instance Hashable (Time t) where
  hashWithSalt = $(structuralHashWithSalt [t|Time|] [])


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
  , vars = [Some (Binder Forall "a" SInt)]
  , prop = Eq SInt
      (Sum (StorageItem Whenever balanceOf [WExpr SInt (Var Forall "a" SInt)]))
      (Read (StorageItem Whenever totalSupply []))
  }

relation :: Relation
relation = Relation
  { name = "no_inflation"
  , prover = Coq
  , vars = [Some (Binder Forall "a" SInt)]
  , prop = Eq SInt
      (Read (StorageItem Pre (StorageLoc "totalSupply" [] SInt) []))
      (Read (StorageItem Post (StorageLoc "totalSupply" [] SInt) []))
  }

constructor :: Constructor
constructor = Constructor
  { params = [Some (Binder X "_name" SStr), Some (Binder X "_symbol" SStr), Some (Binder X "supply" SInt)]
  , branches = Single $ CBranch
      { requires = Set.empty
      , rewrites = Set.fromList
          [
          ]
      , ensures = Set.empty
      }
  }

erc20 :: Contract
erc20 = Contract
  { name = "ERC20"
  , storage = storage
  , invariants = Set.singleton invariant
  , relations = Set.singleton relation
  , constructor = undefined
  , methods = undefined
  }
