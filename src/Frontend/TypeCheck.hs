module Frontend.TypeCheck where

import Data.Map (Map)
import qualified Data.Map as Map

import Javalette.Lex
import Javalette.Par
import Javalette.Skel
import Javalette.Print
import Javalette.Abs
import Javalette.ErrM

-- TODO: Replace Err with Either
type Env =     (Sig, [Context])         -- functions and context stack
type Sig =     Map Ident ([Type], Type) -- function type signature
type Context = Map Ident Type           -- variables with their types

emptyEnv :: Env
emptyEnv = (Map.empty, [])

newBlock :: Env -> Env
newBlock (s, c)  = (s, Map.empty:c)

remBlock :: Env -> Err Env
remBlock (s, []) = Bad "There is no block left."
remBlock (s, c)  = Ok (s, tail c)

lookupVar :: Env -> Ident -> Maybe Type
lookupVar (_, []) _ = Nothing
lookupVar (_, c)  i = lookupV c where
    lookupV (t:r) = case Map.lookup i t of
        Nothing -> lookupV r
        Just y  -> return y

lookupFun :: Env -> Ident -> Maybe ([Type],Type)
lookupFun (s, _) i = Map.lookup i s

extendVar :: Env -> Ident -> Type -> Err Env
extendVar (s, t:r) i y = case Map.lookup i t of
    Nothing -> Ok (s, Map.insert i y t:r)
    Just _  -> Bad ("The variable is already defined: " ++ show i)

extendFun :: Env -> Ident -> ([Type],Type) -> Err Env
extendFun (s, c) i y = case Map.lookup i s of
    Nothing -> Ok (Map.insert i y s, c)
    Just _  -> Bad ("The function is already defined: " ++ show i)

-- TODO: Refactor the function
inferBin :: [Type] -> Env -> Expr -> Expr -> Err Type
inferBin t e x1 x2 = do
    y <- inferExpr e x1
    if y `elem` t
        then checkExpr e x2 y
        else Bad ("Wrong type of expression: " ++ show y)

-- TODO: Add all possible expressions
-- TODO: inferBin for String
inferExpr :: Env -> Expr -> Err Type
inferExpr e x = case x of
    ELitInt i  -> return Int
    ELitDoub d -> return Doub
    ELitTrue   -> return Bool
    ELitFalse  -> return Bool
    EAdd x1 addop x2 -> inferBin [Int, Doub] e x1 x2

-- TODO: Implement the function
checkExpr :: Env -> Expr -> Type -> Err Type
checkExpr e x y = Ok Bool

-- TODO: Implement the following functions
-- check :: Env -> Expr -> Err Type?
-- check :: Env -> [Stmt] -> Err Bool?
-- check :: Program -> Err Bool?

-- TODO: Implement the function
typeCheck :: Program -> Err Program
typeCheck s = Bad "Not implemented!"
