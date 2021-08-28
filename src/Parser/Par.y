{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Parser.Par where
import Abs
import Parser.Lex
import Parser.ErrM
}

%name pITerm ITerm
%name pCTerm CTerm
%name pType Type
%name pListId ListId
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '(' { PT _ (TS _ 1) }
  ')' { PT _ (TS _ 2) }
  '->' { PT _ (TS _ 3) }
  '.' { PT _ (TS _ 4) }
  '::' { PT _ (TS _ 5) }
  'lam' { PT _ (TS _ 6) }

L_Id { PT _ (T_Id $$) }

%%

Id    :: { Id} : L_Id { Id ($1)}

ITerm :: { ITerm }
ITerm : CTerm { Abs.Inf $1 }
      | 'lam' ListId '.' ITerm { Abs.Lam $2 $4 }
      | '(' ITerm ')' { $2 }
CTerm :: { CTerm }
CTerm : ITerm '::' Type { Abs.Ann $1 $3 }
      | Id { Abs.Var $1 }
      | CTerm ITerm { Abs.App $1 $2 }
      | '(' CTerm ')' { $2 }
Type :: { Type }
Type : Id { Abs.TFree $1 }
     | Type '->' Type { Abs.TFun $1 $3 }
     | '(' Type ')' { $2 }
ListId :: { [Id] }
ListId : {- empty -} { [] } | Id ListId { (:) $1 $2 }
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

