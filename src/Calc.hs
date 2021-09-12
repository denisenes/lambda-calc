module Main where

import System.IO
import System.Environment
import System.Exit
import Data.List
import Control.Monad

import Parser.Lex
import Parser.Par
import Parser.ErrM

import Abs

-- ============IO==============
-- TODO: repair file reading

type ParseFun a = [Token] -> Err a
myLLexer = myLexer

run :: Context -> CTerm -> IO ()
run ctx tree = do
    let (Inf new_tree) = prepare tree
    --showTree new_tree
    let res = typecheck ctx new_tree
    case res of
        (Left err) -> putStrLn err
        (Right t) -> do
            let value = eval new_tree
            let vterm = quote_0 value
            putStrLn ("(" ++ (showCTerm 0 vterm) ++ ") :: " ++ (showType t))

showTree :: (Show a) => a -> IO ()
showTree tree = do
    putStrLn ("\nAbstract Syntax:\n\n" ++ show tree)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> io_loop [] --hGetContents stdin >>= run pCTerm
        --fs -> mapM_ (runFile pCTerm) fs

-- Obama wrote this code
io_loop:: Context -> IO ()
io_loop ctx = do
    putStr ">> "
    hFlush stdout
    in_str <- getLine
    case in_str of
        ":by" -> putStrLn "Proko"
        ":c" -> putStrLn $ show ctx
        ":q" -> exitSuccess
        _ -> do
            let ts = myLLexer in_str
            let metaterm = pMetaterm ts
            case metaterm of
                Bad s -> do
                    putStrLn s
                Ok (META_CTERM cterm) -> run ctx cterm
                Ok (META_CONTL (ContL list)) -> let new_ctx = updateContext ctx list in
                    io_loop new_ctx
    io_loop ctx

-- =================TO=DE=BRUIJN=INDICES===============

prepare:: CTerm -> CTerm
prepare term = toIndices_CT emptyStack term

type Stack = [Id]

-- Traverse abstract tree of the term and: 
-- 1) Mark bound variables
--                  v
--       lam x y . (x z)
-- 2) Change names to indices
-- lam x y . (x (lam z . (z y))  ----->  lam . (1 (lam . (0 1)))

emptyStack:: Stack 
emptyStack = []

push:: Stack -> Id -> Stack
push st id = (id:st) 

toIndices_CT:: Stack -> CTerm -> CTerm
toIndices_CT st (Inf iterm) = Inf $ toIndices_IT st iterm
toIndices_CT st (Lam [] it) = toIndices_CT st it
toIndices_CT st (Lam (v:vars) it) = Lam_ (toIndices_CT (push st v) new_lam)
    where
    new_lam = (Lam vars it)


toIndices_IT:: Stack -> ITerm -> ITerm
toIndices_IT st iterm = case iterm of
    Ann cterm type_ -> Ann (toIndices_CT st cterm) (typeToName type_)
    Var (Id id)     -> let res = elem (Id id) st in
        if res then (Bound index) else (Free (Global id))
        where
        (Just index) = elemIndex (Id id) st
    App ct it -> App (toIndices_IT st ct) (toIndices_CT st it)

typeToName:: Type -> Type
typeToName (TFree (Id name)) = (TNFree (Global name))
typeToName (TFun type_ type') = (TFun (typeToName type_) (typeToName type'))

-- =================EVALUATION=================

-- creates the value corresponding to a free variable
vfree :: Name -> Value
vfree n = VNeutral (NFree n)

type Env = [Value]

emptyEnv = []

eval:: ITerm -> Value
eval term = i_eval term emptyEnv

i_eval:: ITerm -> Env -> Value
i_eval (Ann e _) env = c_eval e env
i_eval (Free x) env = vfree x
i_eval (Bound i) env = env !! i
i_eval (App e e') env = vapp (i_eval e env) (c_eval e' env)

vapp:: Value -> Value -> Value
vapp (VLam fun) val = fun val
vapp (VNeutral n) v = VNeutral (NApp n v)

c_eval:: CTerm -> Env -> Value
c_eval (Inf i) env = i_eval i env
c_eval (Lam_ e) env = VLam (\x -> c_eval e (x : env))

-- =================TYPECHECKER=================

debug_env = [(Global "y",HasType (TNFree $ Global "a")),
             (Global "z",HasType (TNFree $ Global "b")),
             (Global "a",HasKind Star)]

data Kind = Star
    deriving (Show)

data Info
        = HasKind Kind
        | HasType Type
    deriving (Show)

type Context =[(Name,Info)]
type Result a = Either String a

updateContext:: Context -> [CTX_Ann] -> Context
updateContext ctx [] = ctx
updateContext ctx ((CTX_Kind (Id id)):anns) = let name = (Global id) in
    updateContext ((name, HasKind Star):ctx) anns
updateContext ctx ((CTX_Type (Id id) type_):anns) = let name = (Global id) in
    updateContext ((name, HasType nt):ctx) anns
    where
    nt = typeToName type_

throwError:: String -> Result a
throwError str = Left $ "Error: " ++ str

typecheck env term = infer_0 env term
--typecheck term = infer_0 [] term

c_kind:: Context -> Type -> Kind -> Result ()
c_kind ctx (TNFree x) Star = case lookup x ctx of
    Just (HasKind Star) -> return ()
    Nothing -> throwError $ "unknown identifier " ++ show x 
c_kind ctx (TFun k k') Star = do
    c_kind ctx k Star
    c_kind ctx k' Star

infer_0:: Context -> ITerm -> Result Type
infer_0 = infer 0

infer:: Int -> Context -> ITerm -> Result Type
infer i ctx (Ann e t) = do
    c_kind ctx t Star
    check i ctx e t
    return t
infer i ctx (Free x) = case lookup x ctx of
    Just (HasType t) -> return t
    Nothing                    -> throwError $ "unknown identifier " ++ show x
infer i ctx (App e e') = do
    res <- infer i ctx e
    case res of
        TFun type_ type' -> do
            check i ctx e' type_
            return type'
        _ -> throwError ("illegal application (" ++ (show e) ++ " " ++ (show e') ++ ")")

check:: Int -> Context -> CTerm -> Type -> Result ()
check i ctx (Inf e) type_ = do
    type' <- infer i ctx e
    unless (type_ == type') (throwError "type mismatch")
check i ctx (Lam_ e) (TFun type_ type') = check (i + 1) ((Local i, HasType type_):ctx)
    (subst_check 0 (Free (Local i)) e) type'
check i ctx _ _ = throwError "type mismatch"

subst_inf:: Int -> ITerm -> ITerm -> ITerm
subst_inf i r (Ann e type_) = Ann (subst_check i r e) type_
subst_inf i r (Bound j) = if i == j then r else Bound j
subst_inf i r (Free y) = Free y
subst_inf i r (App e e') = App (subst_inf i r e) (subst_check i r e')

subst_check:: Int -> ITerm -> CTerm -> CTerm
subst_check i r (Inf e) = Inf (subst_inf i r e)
subst_check i r (Lam_ e) = Lam_ (subst_check (i + 1) r e)

-- =================QUOTATION=================
-- converts value to a term to print the value

boundfree :: Int -> Name -> ITerm
boundfree i (Quote k) = Bound (i - k - 1)
boundfree i x = Free x

quote_0:: Value -> CTerm
quote_0 = quote 0

quote:: Int -> Value -> CTerm
quote i (VLam f) = Lam_ (quote (i + 1) (f (vfree (Quote i))))
quote i (VNeutral n) = Inf (neutralQuote i n)

neutralQuote:: Int -> Neutral -> ITerm
neutralQuote i (NFree x) = boundfree i x
neutralQuote i (NApp n v) = App (neutralQuote i n) (quote i v)

-- ==================PRINTER==================

showType :: Type -> String
showType (TNFree (Global str)) = str
showType (TFun t t') = "(" ++ (showType t) ++ " -> " ++ (showType t') ++ ")" 

showCTerm :: Int -> CTerm -> String
showCTerm i (Inf it) = showITerm i it
showCTerm i (Lam_ ct) = "lam x" ++ (show i) ++ " . " ++ (showCTerm (i+1) ct)

showITerm :: Int -> ITerm -> String
showITerm i (Ann ct _) = showCTerm i ct
showITerm i (Free (Global name)) = name
showITerm i (Bound d) = "x"++show (i - d - 1)
showITerm i (App term term') = "("++ (showITerm i term) ++" "++ (showCTerm i term') ++")"
