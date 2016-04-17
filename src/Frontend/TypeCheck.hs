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
import Control.Arrow

import Javalette.Abs

import Frontend.Types
import Frontend.Query
import Frontend.ReturnCheck

import Utils.Error

-- temporary:
import Frontend.Example
import Utils.Debug

type FnSigId = (Ident, FnSig)

typeCheck :: Program -> Err Env
typeCheck prog = do
    env  <- allFunctions prog
    env2 <- checkProg env prog
    return env2

mainExists :: Env -> Err Env
mainExists env = do
    (args, rtype) <- lookupFun' env $ Ident "main"
    case rtype == Int && null args of
        True  -> return env
        False -> Left $ wrgFunSig $ Ident "main"

allFunctions :: Program -> Err Env
allFunctions = collectFuns emptyEnv . (predefFuns ++) . progFunSigs

predefFuns :: [FnSigId]
predefFuns = map (first Ident)
             [("printInt",    ([Int],      Void)),
              ("printDouble", ([Doub],     Void)),
              ("printString", ([ConstStr], Void)),
              ("readInt",     ([],         Int)),
              ("readDouble",  ([],         Doub))]

progFunSigs :: Program -> [FnSigId]
progFunSigs = map toFnSigId . progFuns

collectFuns :: Env -> [FnSigId] -> Err Env
collectFuns = foldM extendFun

toFnSigId :: TopDef -> FnSigId
toFnSigId (FnDef ret id args _) = (id, (map argType args, ret))

extendFun :: Env -> FnSigId -> Err Env
extendFun (sigs, ctxs) (ident, sig) =
    case Map.lookup ident sigs of
    Nothing -> Right (Map.insert ident sig sigs, ctxs)
    Just _  -> Left $ funcAlrDef ident

checkProg :: Env -> Program -> Err Env
checkProg env = foldM checkFunc env . progFuns

emptyEnv :: Env
emptyEnv = (Map.empty, [Map.empty])

newBlock :: Env -> Env
newBlock (sigs, ctxs)  = (sigs, Map.empty:ctxs)

remBlock :: Env -> Env
remBlock (sigs, []) = (sigs, [])
remBlock (sigs, ctxs)  = (sigs, tail ctxs)

lookupVar :: Env -> Ident -> Maybe Type
lookupVar (_, [])       _     = Nothing
lookupVar (_, contexts) ident = lookupVarH contexts ident

-- TODO: Implement with find
lookupVarH :: [Context] -> Ident -> Maybe Type
lookupVarH []     _     = Nothing
lookupVarH (c:cs) ident = case Map.lookup ident c of
    Nothing  -> lookupVarH cs ident
    Just typ -> return typ

lookupVar' :: Env -> Ident -> Err Type
lookupVar' env ident = case lookupVar env ident of
    Just typ -> return typ
    Nothing  -> Left $ varNotDef ident

extendVar :: Env -> Ident -> Type -> Err Env
extendVar (sigs, c:cs) ident typ = case Map.lookup ident c of
    Nothing -> Right (sigs, Map.insert ident typ c:cs)
    Just _  -> Left $ varAlrDef ident

lookupFun :: Env -> Ident -> Maybe FnSig
lookupFun (sigs, _) ident = Map.lookup ident sigs

lookupFun' :: Env -> Ident -> Err FnSig
lookupFun' env ident = case lookupFun env ident of
        Just sig -> return sig
        Nothing  -> Left $ funcNotDef ident

-- TODO: inferBin for String
inferExp :: Env -> Expr -> Err Type
inferExp env expr = case expr of
    EVar ident        -> lookupVar' env ident
    ELitInt int       -> return Int
    ELitDoub doub     -> return Doub
    ELitTrue          -> return Bool
    ELitFalse         -> return Bool
    EApp ident xps    -> inferFun env ident xps
    EString str       -> return ConstStr
    Neg xpr           -> inferUnary env [Int, Doub] xpr
    Not xpr           -> inferUnary env [Bool] xpr
    EMul left o right -> inferBinary env (mulOperator o) left right
    EAdd left _ right -> inferBinary env [Int, Doub] left right
    ERel left o right -> inferRel env (relOperator o) left right
    EAnd left right   -> inferBinary env [Bool] left right
    EOr left right    -> inferBinary env [Bool] left right

relOperator oper | oper `elem` [NE, EQU] = [Int, Doub, Bool]
                 | otherwise             = [Int, Doub]

mulOperator oper | oper == Mod = [Int]
                 | otherwise   = [Int, Doub]

-- TODO: Refactor the function: Remove do
inferBinary :: Env -> [Type] -> Expr -> Expr -> Err Type
inferBinary env types exp1 exp2 = do
    typl <- inferExp env exp1
    typr <- inferExp env exp2
    case typl `elem` types of
        True  -> checkExp env typl exp2
        False -> Left $ wrgBinExp exp1 exp2 typl typr

-- TODO: Merge with inferBinary
inferRel :: Env -> [Type] -> Expr -> Expr -> Err Type
inferRel env types exp1 exp2 = do
    typl <- inferExp env exp1
    typr <- inferExp env exp2
    case typl == typr && typl `elem` types of
        True  -> return Bool
        False -> Left $ wrgRelExp exp1 exp2 typl typr

inferUnary :: Env -> [Type] -> Expr -> Err Type
inferUnary env types expr = do
    typ <- inferExp env expr
    case typ `elem` types of
        True  -> return typ
        False -> Left $ wrgUnaExp expr types typ

inferFun :: Env -> Ident -> [Expr] -> Err Type
inferFun env ident exprs = do
    (argtypes, rtype) <- lookupFun' env ident
    exprtypes         <- mapM (inferExp env) exprs
    case argtypes == exprtypes of
        True  -> return rtype
        False -> Left $ wrgArgTyp ident argtypes exprtypes

checkFunc :: Env -> TopDef -> Err Env
checkFunc env (FnDef rtype ident args block) = do
    env2 <- foldM (\env (Arg typ aident) ->
                   extendVar env aident typ) (newBlock env) args
    checkBlock env2 rtype block

checkBlock :: Env -> Type -> Block -> Err Env
checkBlock env typ (Block block) =
    remBlock <$> debug <$> checkStms env typ block

checkStms :: Env -> Type -> [Stmt] -> Err Env
checkStms env typ = foldM (`checkStm` typ) env

checkStm :: Env -> Type -> Stmt -> Err Env
checkStm env typ stmt = case stmt of
    Empty               -> return env
    BStmt block         -> checkBlock (newBlock env) typ block
    Decl dtyp items     -> foldM (checkDecl dtyp) env items
    Ass ident expr      -> checkExp' env ident expr >> return env
    Incr ident          -> checkIdent env [Int, Doub] ident >> return env
    Decr ident          -> checkIdent env [Int, Doub] ident >> return env
    Ret expr            -> checkExp env typ expr >> return env
    VRet                -> checkVoid env typ >> return env
    Cond expr st        -> checkExp env Bool expr >> checkStm env typ st
    CondElse expr s1 s2 -> checkExp env Bool expr >> checkStms env typ [s1, s2]
    While expr st       -> checkExp env Bool expr >> checkStm env typ st
    SExp expr           -> const env <$> inferExp env expr

checkExp :: Env -> Type -> Expr -> Err Type
checkExp env ty1 expr = do
    ty2 <- inferExp env expr
    case ty2 == ty1 of
        True  -> return ty2
        False -> Left $ wrgExpTyp expr ty1 ty2

checkExp' :: Env -> Ident -> Expr -> Err Type
checkExp' env ident expr = do
    ty1 <- lookupVar' env ident
    ty2 <- inferExp env expr
    case ty1 == ty2 of
        True  -> return ty1
        False -> Left $ wrgIdeTyp ident ty1 ty2

checkIdent :: Env -> [Type] -> Ident -> Err Type
checkIdent env types ident = do
    typ <- lookupVar' env ident
    case typ `elem` types of
        True  -> return typ
        False -> Left $ wrgIdeTyp' ident types typ

checkVoid :: Env -> Type -> Err Type
checkVoid env typ = do
    case typ == Void  of
        True  -> return typ
        False -> Left $ wrgVoidTyp typ

checkDecl :: Type -> Env -> Item -> Err Env
checkDecl typ env item = do
    ident <- case item of
             Init ident expr -> checkExp env typ expr >> return ident
             NoInit ident    -> return ident
    extendVar env ident typ
