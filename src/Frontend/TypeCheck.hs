{- Javalette Compiler
   Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -}

{-|
Module      : Frontend.TypeCheck
Description : Type checker for Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
Stability   : experimental
Portability : ALL

Type checker for Javalette compiler.
-}
module Frontend.TypeCheck (
    -- * Types
    Env, Sig, Context, Err, Log,

    -- * Operations
    typeCheck
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad

import Javalette.Lex
import Javalette.Par
import Javalette.Skel
import Javalette.Print
import Javalette.Abs

import Frontend.Types

emptyEnv :: Env
emptyEnv = (Map.empty, [])

newBlock :: Env -> Env
newBlock (s, c)  = (s, Map.empty:c)

remBlock :: Env -> Err Env
remBlock (s, []) = Left "There is no block left."
remBlock (s, c)  = Right (s, tail c)

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
    Nothing -> Right (s, Map.insert i y t:r)
    Just _  -> Left ("The variable is already defined: " ++ show i)

extendFun :: Env -> Ident -> ([Type],Type) -> Err Env
extendFun (s, c) i y = case Map.lookup i s of
    Nothing -> Right (Map.insert i y s, c)
    Just _  -> Left ("The function is already defined: " ++ show i)

-- TODO: Add all possible expressions
-- TODO: inferBin for String
inferExp :: Env -> Expr -> Err Type
inferExp e x = case x of
    ELitInt i  -> return Int
    ELitDoub d -> return Doub
    ELitTrue   -> return Bool
    ELitFalse  -> return Bool
    EAdd x1 addop x2 -> inferBin [Int, Doub] e x1 x2

-- TODO: Refactor the function: Remove do
inferBin :: [Type] -> Env -> Expr -> Expr -> Err Type
inferBin t e x1 x2 = do
    y <- inferExp e x1
    if y `elem` t
        then checkExp e y x2
        else Left ("Wrong type of expression: " ++ show y)

-- TODO: Refactor the function: Remove do
checkExp :: Env -> Type -> Expr -> Err Type
checkExp e y1 x = do
    y2 <- inferExp e x
    if y2 == y1 then Right y2
        else Left $ unwords ["Expected type", show y1, "for", show e,
                             "but found:", show y2]

-- TODO: Refactor the function: Remove do
-- TODO: Do we need a type here?
checkStm :: Env -> Stmt -> Err Env
checkStm e s = case s of
    SExp x -> do
        inferExp e x
        return e
    Decl z s -> return e
        -- TODO: Check if this should be updateVar
        -- TODO: find the id somehow
        -- extendVar e i z
    While x s -> do
        checkExp e Bool x
        checkStm e s

checkStms :: Env -> [Stmt] -> Err Env
checkStms = foldM checkStm

-- TODO: Implement: Consider Abs.hs for the data types
checkProg :: Env -> Program -> Err Env
checkProg e p = Right e

-- TODO: Implement the function
typeCheck :: Program -> Err Program
typeCheck s = Left "Not implemented!"
