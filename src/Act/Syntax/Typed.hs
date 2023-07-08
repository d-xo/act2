{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeFamilies #-}

module Act.Syntax.Typed where

import Data.Text (Text)
import Data.Sequence (Seq)
import Data.HashMap (Map)
import Data.Set (Set)
import Data.Parameterized.Some (Some)
import Data.Parameterized.List (List)


--- Contracts --------------------------------------------------------------------------------------


-- | Top level type representing a full specification of a contract
data Contract = Contract
  { name :: Text
  , storage :: StorageLayout
  , invariants :: Set Invariant
  , constructor :: Constructor
  , methods :: Set (Some Method)
  }


--- Behaviours -------------------------------------------------------------------------------------


-- | Initcode spec
data Constructor = Constructor
  { name :: Text
  , args :: Seq (Some (Binder Unquantified))
  , spec :: Cases CBranch
  }

-- | A description of a single branch of a constructor execution
data CBranch = CBranch
  { requires :: Set (Expr Unquantified Timed ABool)
  , rewrites :: Set (Some (Rewrite Unquantified))
  , ensures :: Set (Expr Unquantified Timed ABool)
  }

-- | Method spec
data Method (a :: Ty) = Method
  { name :: Text
  , args :: Seq (Some (Binder Unquantified))
  , spec :: Cases (Branch a)
  , ret  :: STy a
  }

-- | A description of a single branch of a method execution
data Branch (a :: Ty) = Branch
  { requires :: Set (Expr Unquantified Timed ABool)
  , rewrites :: Set (Some (Rewrite Unquantified))
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
newtype StorageLayout = StorageLayout (Seq (Some (Loc Unquantified)))

-- | Existential wrapper to make StorageLayout typecheck
data Loc (q :: Quantity) (a :: Ty) where
  Loc :: Some (StorageLoc q a) -> Loc q a

-- | An assignment from the rhs to the lhs
data Rewrite (q :: Quantity) (a :: Ty) where
  Rewrite ::
    { lhs :: Some (StorageLoc q a)
    , rhs :: Expr q Timed a
    } -> Rewrite q a

-- | A pointer to a location in storage
data StorageLoc (q :: Quantity) (a :: Ty) (as :: [Ty]) where
  StorageLoc ::
    { args :: List (Binder q) as
    , ret :: STy a
    } -> StorageLoc q a as

-- | The result of reading an item from storage
data StorageItem (q :: Quantity) (t :: Timing) (a :: Ty) where
  StorageItem ::
    { time :: Time t
    , loc  :: StorageLoc q a as
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
  , args :: Seq (Some (Binder Quantified))
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
