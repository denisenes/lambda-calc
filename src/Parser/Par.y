-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Parser.Par where
import Abs
import Parser.Lex
import Parser.ErrM

}

%name pMetaterm Metaterm
%name pCTX_Ann CTX_Ann
%name pITerm ITerm
%name pCTerm CTerm
%name pType Type
%name pListId ListId
%name pCTX_List CTX_List
%name pListCTX_Ann ListCTX_Ann
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '(' { PT _ (TS _ 1) }
  ')' { PT _ (TS _ 2) }
  '*' { PT _ (TS _ 3) }
  '->' { PT _ (TS _ 4) }
  '.' { PT _ (TS _ 5) }
  '::' { PT _ (TS _ 6) }
  'assume' { PT _ (TS _ 7) }
  'lam' { PT _ (TS _ 8) }

L_Id { PT _ (T_Id $$) }


%%

Id    :: { Id} : L_Id { Id ($1)}

Metaterm :: { Metaterm }
Metaterm : CTerm { Abs.META_CTERM $1 }
         | CTX_List { Abs.META_CONTL $1 }
CTX_Ann :: { CTX_Ann }
CTX_Ann : '(' CTX_Ann ')' { $2 }
        | Id '::' '*' { Abs.CTX_Kind $1 }
        | Id '::' Type { Abs.CTX_Type $1 $3 }
ITerm :: { ITerm }
ITerm : CTerm '::' Type { Abs.Ann $1 $3 }
      | Id { Abs.Var $1 }
      | ITerm CTerm { Abs.App $1 $2 }
      | '(' ITerm ')' { $2 }
CTerm :: { CTerm }
CTerm : ITerm { Abs.Inf $1 }
      | 'lam' ListId '.' CTerm { Abs.Lam $2 $4 }
      | '(' CTerm ')' { $2 }
Type :: { Type }
Type : Id { Abs.TFree $1 }
     | Type '->' Type { Abs.TFun $1 $3 }
     | '(' Type ')' { $2 }
ListId :: { [Id] }
ListId : {- empty -} { [] } | Id ListId { (:) $1 $2 }
CTX_List :: { CTX_List }
CTX_List : 'assume' ListCTX_Ann { Abs.ContL (reverse $2) }
ListCTX_Ann :: { [CTX_Ann] }
ListCTX_Ann : {- empty -} { [] }
            | ListCTX_Ann CTX_Ann { flip (:) $1 $2 }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}
