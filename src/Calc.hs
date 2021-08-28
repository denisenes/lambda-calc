module Main where

import System.IO
import System.Environment
import System.Exit

import Parser.Lex
import Parser.Par
import Parser.ErrM
import Abs

-- ============IO==============

type ParseFun a = [Token] -> Err a
myLLexer = myLexer

runFile :: ParseFun ITerm -> FilePath -> IO ()
runFile p f = putStrLn f >> readFile f >>= run p

run :: ParseFun ITerm -> String -> IO ()
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
    [] -> hGetContents stdin >>= run pITerm
    fs -> mapM_ (runFile pITerm) fs

-- =================TO=DE=BRUIJN=INDICES===============
prepare:: ITerm -> ITerm
prepare term = toIndices (markVars term)

-- 1) Mark bound variables
--                 v
--      lam x y . (x z)

markVars:: ITerm -> ITerm
markVars iterm = iterm         --TODO

-- 2) Change names to indices
--      lam x y . x  -----> lam 1

toIndices:: ITerm -> ITerm
toIndices iterm = iterm        --TODO