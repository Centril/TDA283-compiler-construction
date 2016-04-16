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
import Utils.Debug

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

remBlock :: Env -> Env
remBlock (s, []) = (s, [])
remBlock (s, c)  = (s, tail c)

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
    Just _  -> Left $ "The variable is already defined: " ++ show i

lookupVar' :: Env -> Ident -> Err Type
lookupVar' env ident = case lookupVar env ident of
        Just typ -> return typ
        Nothing  -> Left $ "The variable is not defined: " ++ show ident

-- TODO: inferBin for String
inferExp :: Env -> Expr -> Err Type
inferExp env expr = case expr of
    EVar ident        -> lookupVar' env ident
    ELitInt int       -> return Int
    ELitDoub doub     -> return Doub
    ELitTrue          -> return Bool
    ELitFalse         -> return Bool
    EApp ident xps    -> Left "Not implemented"
    EString str       -> return ConstStr
    Neg xpr           -> inferUnary env [Int, Doub] xpr
    Not xpr           -> inferUnary env [Bool] xpr
    EMul left _ right -> inferBinary env [Int, Doub] left right
    EAdd left _ right -> inferBinary env [Int, Doub] left right
    ERel left _ right -> inferBinary env [Int, Doub] left right
    EAnd left right   -> inferBinary env [Bool] left right
    EOr left right    -> inferBinary env [Bool] left right

-- TODO: Refactor the function: Remove do
inferBinary :: Env -> [Type] -> Expr -> Expr -> Err Type
inferBinary e t x1 x2 = do
    y <- inferExp e x1
    case y `elem` t of
        True  -> checkExp e y x2
        False -> Left $ "Wrong type of expression: " ++ show y

inferUnary :: Env -> [Type] -> Expr -> Err Type
inferUnary e t x1 = do
    y <- inferExp e x1
    case y `elem` t of
        True  -> return y
        False -> Left $ "Wrong type of expression: " ++ show y

-- TODO: Refactor the function: Remove do
checkExp :: Env -> Type -> Expr -> Err Type
checkExp env ty1 x = do
    ty2 <- inferExp env x
    case ty2 == ty1 of
        True  -> return ty2
        False -> Left $ unwords ["Expected type", show ty1, "for", show env,
                                 "but found:", show ty2]

checkExp' :: Env -> Ident -> Expr -> Err Type
checkExp' env ident expr = do
    ty1 <- lookupVar' env ident
    ty2 <- inferExp env expr
    case ty1 == ty2 of
        True  -> return ty1
        False -> Left $ unwords ["Expected type", show ty1, "for",
                                 show ident, "but found:", show ty2]

checkIdent :: Env -> [Type] -> Ident -> Err Type
checkIdent env types ident = do
    typ <- lookupVar' env ident
    case typ `elem` types of
        True  -> return typ
        False -> Left $ "Wrong type for ident: " ++ show ident

checkVoid :: Env -> Type -> Err Type
checkVoid env typ = do
    case typ == Void  of
        True  -> return typ
        False -> Left $ "Wrong type for void: " ++ show typ

-- TODO: Refactor the function: Remove do
checkStm :: Env -> Type -> Stmt -> Err Env
checkStm env typ stmt = case stmt of
    Empty               -> return env
    BStmt block         -> checkBlock env typ block
    Decl typ item       ->
        foldM (\en iden -> extendVar en iden typ) env (itemIdent <$> item)
    Ass ident expr      -> do
        checkExp' env ident expr
        return env
    Incr ident          -> do
        checkIdent env [Int, Doub] ident
        return env
    Decr ident          -> do
        checkIdent env [Int, Doub] ident
        return env
    Ret expr            -> do
        checkExp env typ expr
        return env
    VRet                -> do
        checkVoid env typ
        return env
    Cond expr st        -> do
        checkExp env Bool expr
        checkStm env typ st
    CondElse expr s1 s2 -> do
        checkExp env Bool expr
        checkStm env typ s1
        checkStm env typ s2
    While expr st       -> do
        checkExp env Bool expr
        checkStm env typ st
    SExp expr           -> const env <$> inferExp env expr

itemIdent :: Item -> Ident
itemIdent (Init i l) = i
itemIdent (NoInit i) = i

checkStms :: Env -> Type -> [Stmt] -> Err Env
checkStms e y = foldM (`checkStm` y) e

checkBlock :: Env -> Type -> Block -> Err Env
checkBlock env typ (Block block) =
    remBlock <$> debug <$> checkStms (newBlock env) typ block

checkFunc :: Env -> TopDef -> Err Env
checkFunc e (FnDef y i a b) = checkBlock e y b

checkProg :: Env -> Program -> Err Env
checkProg e (Program t) = checkFunc e (head t)
