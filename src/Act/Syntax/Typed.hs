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


--- Contracts --------------------------------------------------------------------------------------


-- | Top level type representing a full specification of a contract
data Contract = Contract
  { name :: Text
  , storage :: StorageLayout
  , invariants :: Set Invariant
  , constructor :: Constructor
  , methods :: Set Method
  }

-- | Initcode spec
data Constructor = Constructor
  { name :: Text
  , args :: Seq (Some Binder)
  , spec :: Cases CBranch
  }

-- | A description of a single branch of a constructor execution
data CBranch = CBranch
  { requires :: Set (Expr ABool)
  , rewrites :: Set (Some Rewrite)
  , ensures :: Set (Expr ABool)
  }

-- | Method spec
data Method = Method
  { name :: Text
  , args :: Seq (Some Binder)
  , spec :: Cases Branch
  }

-- | A description of a single branch of a method execution
data Branch = Branch
  { requires :: Set (Expr ABool)
  , rewrites :: Set (Some Rewrite)
  , ensures :: Set (Expr ABool)
  , returns :: Some Expr
  }

-- | Potentially multiple execution branches
data Cases s
  = Cases (Map (Expr ABool) (Cases s)) s
  | Single s

-- | Introduce a new name
data Binder ty = Binder
  { name :: Text
  , ty :: STy ty
  }


--- Storage ----------------------------------------------------------------------------------------


-- | Layout of variables in storage
newtype StorageLayout = StorageLayout (Seq (Some StorageLoc))

-- | An assignment from the rhs to the lhs
data Rewrite t = Rewrite
  { lhs :: StorageItem Post t
  , rhs :: Expr t
  }

-- | A pointer to a location in storage
data StorageLoc t = StorageLoc
  { args :: Seq (Some Binder)
  , ret :: STy t
  }

-- | The result of reading an item from storage
data StorageItem (p :: Time) (ty :: Ty) where
  StorageItem :: { time :: STime p , loc  :: StorageLoc ty , args :: Seq (Some Expr) } -> StorageItem p ty

-- | Was a storage item read from the pre or post state?
data Time
  = Pre
  | Post

-- | Singleton for Time
data STime (t :: Time) where
  SPre  :: STime Pre
  SPost :: STime Post


--- Invariants -------------------------------------------------------------------------------------


-- | A contract level invariant
data Invariant = Invariant
  { name :: Text
  , prover :: Prover
  , args :: Seq (Quantifier, Some Binder)
  , prop :: Expr ABool
  }

-- | How should we prove this invariant?
data Prover
  = SMT
  | Coq

-- | Variables in invariants are quantified
data Quantifier
  = Forall
  | Exists


--- Expressions ------------------------------------------------------------------------------------


-- | Expression types
data Ty
  = AInt
  | ABool
  | AStr

-- | Core expression types
data Expr (ty :: Ty) where
  -- Variables and literals
  Lit :: STy ty -> LitRep ty -> Expr ty
  Var :: STy ty -> Text -> Expr ty
  Read :: StorageItem p t -> Expr t

  -- Integer arithmetic
  Add :: Expr AInt -> Expr AInt -> Expr AInt
  Sub :: Expr AInt -> Expr AInt -> Expr AInt
  Mul :: Expr AInt -> Expr AInt -> Expr AInt
  Div :: Expr AInt -> Expr AInt -> Expr AInt
  Mod :: Expr AInt -> Expr AInt -> Expr AInt
  Exp :: Expr AInt -> Expr AInt -> Expr AInt

  -- Boolean
  And :: Expr ABool -> Expr ABool -> Expr ABool
  Or  :: Expr ABool -> Expr ABool -> Expr ABool
  Not :: Expr ABool -> Expr ABool -> Expr ABool
  LT  :: Expr AInt  -> Expr AInt  -> Expr ABool
  GT  :: Expr AInt  -> Expr AInt  -> Expr ABool
  GEq :: Expr AInt  -> Expr AInt  -> Expr ABool
  LEq :: Expr AInt  -> Expr AInt  -> Expr ABool
  Eq  :: STy a -> Expr a -> Expr a -> Expr ABool
  Imp :: Expr ABool -> Expr ABool -> Expr ABool

  -- constrains an integer expression to have a finite range
  -- needed because we encode this differently in different backends
  Fin ::
    { min :: LitRep AInt
    , max :: LitRep AInt
    , exp :: Expr AInt
    } -> Expr ABool

-- | The haskell type used to represent literals
type family LitRep (t :: Ty) where
  LitRep AInt = Integer
  LitRep ABool = Bool
  LitRep AStr = Text

-- | Singleton for Ty
data STy (t :: Ty) where
  SInt  :: STy AInt
  SBool :: STy ABool
  SStr  :: STy AStr
