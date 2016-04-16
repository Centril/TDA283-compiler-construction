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
    Just _  -> Left $ "The variable is already defined: " ++ show i

-- TODO: inferBin for String
inferExp :: Env -> Expr -> Err Type
inferExp env expr = case expr of
    EVar ident        -> case lookupVar env ident of
        Just typ      -> return typ
        Nothing       -> Left $ "The variable is not defined: " ++ show ident
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
checkExp e y1 x = do
    y2 <- inferExp e x
    case y2 == y1 of
        True  -> return y2
        False -> Left $ unwords ["Expected type", show y1, "for", show e,
                             "but found:", show y2]

-- TODO: Refactor the function: Remove do
checkStm :: Env -> Type -> Stmt -> Err Env
checkStm env typ stmt = case stmt of
    Empty               -> Left "Not implemented"
    BStmt block         -> Left "Not implemented"
    Decl typ item       ->
        foldM (\en iden -> extendVar en iden typ) env (itemIdent <$> item)
    Ass ident expr      -> Left "Not implemented"
    Incr ident          -> Left "Not implemented"
    Decr ident          -> Left "Not implemented"
    Ret expr            -> do
        checkExp env typ expr
        return env
    VRet                -> Left "Not implemented"
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
checkBlock e y (Block b) = checkStms e y b

checkFunc :: Env -> TopDef -> Err Env
checkFunc e (FnDef y i a b) = checkBlock e y b

checkProg :: Env -> Program -> Err Env
checkProg e (Program t) = checkFunc e (head t)
