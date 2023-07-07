{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeFamilies #-}

module Act.Syntax.Typed where

import Data.Text
import Data.Sequence
  import Data
import Data.Parameterized.Some

data Ty
  = AInt
  | ABool
  | AStr

type family LitRep (t :: Ty) where
  LitRep AInt = Integer
  LitRep ABool = Bool
  LitRep AStr = Text

data STy (t :: Ty) where
  SInt  :: STy AInt
  SBool :: STy ABool
  SStr  :: STy AStr

data STime (t :: Time) where
  SPre :: STime Pre
  SPost :: STime Post

data Time
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

data Cases s
  = Cases (Map Prop s) s
  | Single s

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
  { lhs :: StorageItem Post t
  , rhs :: Expr t
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
  , ret :: STy t
  }

data Contract = Contract
  { name :: Text
  , storage :: StorageLayout
  , invariants :: Seq Invariant
  , constructor :: Constructor
  , methods :: Seq Method
  }
