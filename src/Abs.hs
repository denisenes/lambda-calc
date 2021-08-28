module Abs where

newtype Id = Id String deriving (Eq, Ord, Show, Read)

data ITerm = Inf CTerm
    | Lam [Id] ITerm
  deriving (Eq, Ord, Show, Read)

data CTerm = Ann ITerm Type
    | Var Id
    | App CTerm ITerm
  deriving (Eq, Ord, Show, Read)

data Type = TFree Id
    | TFun Type Type
  deriving (Eq, Ord, Show, Read)

