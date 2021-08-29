module Main where

import System.IO
import System.Environment
import System.Exit
import Data.List

import Parser.Lex
import Parser.Par
import Parser.ErrM
import Abs

-- ============IO==============

type ParseFun a = [Token] -> Err a
myLLexer = myLexer

runFile :: ParseFun CTerm -> FilePath -> IO ()
runFile p f = putStrLn f >> readFile f >>= run p

run :: ParseFun CTerm -> String -> IO ()
run p s = let ts = myLLexer s in case p ts of
  Bad s -> do
    putStrLn s
    exitFailure
  Ok  tree -> do
    let new_tree = prepare tree
    showTree tree
    showTree new_tree
    exitSuccess


showTree :: (Show a) => a -> IO ()
showTree tree = do
  putStrLn ("\n[Abstract Syntax]\n\n" ++ show tree)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> hGetContents stdin >>= run pCTerm
    fs -> mapM_ (runFile pCTerm) fs

-- =================TO=DE=BRUIJN=INDICES===============
prepare:: CTerm -> CTerm
prepare term = toIndices_CT emptyStack term

type Stack = [Id]

-- Traverse abstract tree of the term and: 
-- 1) Mark bound variables
--                 v
--      lam x y . (x z)
-- 2) Change names to indices
--      lam x y . (x (lam z . (z y))  -----> lam lam ()

emptyStack:: Stack 
emptyStack = []

push:: Stack -> Id -> Stack
push st id = (id:st) 

toIndices_CT:: Stack -> CTerm -> CTerm
toIndices_CT st (Inf iterm) = Inf $ toIndices_IT st iterm
toIndices_CT st (Lam vars it) = Lam_ (toIndices_CT (update st vars) it)
  where
  update st [] = st
  update st (id:tail) = update (id:st) tail


toIndices_IT:: Stack -> ITerm -> ITerm
toIndices_IT st iterm = case iterm of
  Ann cterm type_ -> Ann (toIndices_CT st cterm) type_
  Var (Id id)  -> let res = elem (Id id) st in
    if res then (Bound index) else (Free (Global id))
    where
    (Just index) = elemIndex (Id id) st
  App ct it -> App (toIndices_IT st ct) (toIndices_CT st it)


-- =================EVALUATION=================

-- creates the value corresponding to a free variable
vfree :: Name -> Value
vfree n = VNeutral (NFree n)

type Env = [Value]

i_eval:: ITerm -> Env -> Value
i_eval (Ann e _) d = c_eval e d
i_eval (Free x) d = vfree x
i_eval (Bound i) d = d !! i
i_eval (App e e') d = vapp (i_eval e d) (c_eval e' d)

--c_eval:: CTerm -> Env -> Value
--c_eval = ...

--vapp:: Value -> Value -> Value
--vapp = ...