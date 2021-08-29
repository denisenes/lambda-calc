module Abs where

newtype Id = Id String deriving (Eq, Ord, Show, Read)

data CTerm 
    = Inf ITerm
    | Lam [Id] CTerm          -- before de Bruijn indexing
    | Lam_ CTerm              
  deriving (Eq, Ord, Show, Read)

data ITerm 
    = Ann CTerm Type
    | Var Id                  -- before de Bruijn indexing        
    | Free Name               
    | Bound Int               
    | App ITerm CTerm
  deriving (Eq, Ord, Show, Read)

data Name
    = Global String
    | Local Int
    | Quote Int
  deriving (Show,Eq, Ord, Read) 

data Type = TFree Id
    | TFun Type Type
  deriving (Eq, Ord, Show, Read)

data Value
    = VLam (Value -> Value)
    | VNeutral Neutral

data Neutral
    = NFree Name
    | NApp Neutral Value