module Abs where

newtype Id = Id String deriving (Eq, Ord, Show, Read)

data Metaterm = META_CTERM CTerm | META_CONTL CTX_List
  deriving (Eq, Ord, Show, Read)

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

data Type
    = TFree Id
    | TNFree Name
    | TFun Type Type
  deriving (Eq, Ord, Show, Read)

data Value
    = VLam (Value -> Value)
    | VNeutral Neutral
  deriving (Show)

data Neutral
    = NFree Name
    | NApp Neutral Value
  deriving (Show)

instance Show (a -> b) where
  show f = "Func "

data CTX_List = ContL [CTX_Ann]
  deriving (Eq, Ord, Show, Read)

data CTX_Ann = CTX_Kind Id | CTX_Type Id Type
  deriving (Eq, Ord, Show, Read)