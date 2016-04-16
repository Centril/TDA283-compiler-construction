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
import Frontend.Query

-- temporary:
import Frontend.Example

typeCheck :: Program -> Err Env
typeCheck = checkProg emptyEnv

collectFuns :: Env -> Program -> Err Env
collectFuns env = foldM extendFun' env . progFuns

extendFun' :: Env -> TopDef -> Err Env
extendFun' env (FnDef ret id args _) =
    extendFun env id (map argType args, ret)

extendFun :: Env -> Ident -> FnSig -> Err Env
extendFun (s, c) i y = case Map.lookup i s of
    Nothing -> Right (Map.insert i y s, c)
    Just _  -> Left ("The function is already defined: " ++ show i)

emptyEnv :: Env
emptyEnv = (Map.empty, [Map.empty])

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

-- TODO: Add all possible expressions
-- TODO: inferBin for String
inferExp :: Env -> Expr -> Err Type
inferExp e x = case x of
    ELitInt i  -> return Int
    ELitDoub d -> return Doub
    ELitTrue   -> return Bool
    ELitFalse  -> return Bool
    EVar i     -> case lookupVar e i of
        Just y  -> Right y
        Nothing -> Left ("The variable is not yet defined: " ++ show i)
    EAdd x1 addop x2 -> inferBin e [Int, Doub] x1 x2
    _ -> Left $ unwords ["Expression", show x, "not implemented!"]

-- TODO: Refactor the function: Remove do
inferBin :: Env -> [Type] -> Expr -> Expr -> Err Type
inferBin e t x1 x2 = do
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
checkStm :: Env -> Type -> Stmt -> Err Env
checkStm e y s = case s of
    SExp x -> const e <$> inferExp e x
    Decl z b ->
        foldM (\x y -> extendVar x y z) e (itemIdent <$> b)
    While x s -> do
        checkExp e Bool x
        checkStm e y s
    Ret x -> do
        z <- inferExp e x
        if z == y then Right e
            else Left $ unwords ["Expected type", show y,
                                     "for return value", show e,
                                     "but found:", show z]
    _ -> Left $ unwords ["Statement", show s, "not implemented!"]

itemIdent :: Item -> Ident
itemIdent (Init i l) = i
itemIdent (NoInit i) = i

checkStms :: Env -> Type -> [Stmt] -> Err Env
checkStms e y = foldM (`checkStm` y) e

checkBlock :: Env -> Type -> Block -> Err Env
checkBlock e y (Block b) = checkStms e y b

checkFunc :: Env -> TopDef -> Err Env
checkFunc e (FnDef y i a b) = checkBlock e y b

checkProg :: Env -> Program -> Err Env
checkProg e (Program t) = checkFunc e (head t)
