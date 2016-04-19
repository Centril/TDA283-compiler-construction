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

import qualified Data.Map as Map

import Control.Monad
import Control.Arrow

import Utils.Foldable

import Javalette.Abs

import Frontend.Types
import Frontend.Query
import Frontend.ReturnCheck
import Frontend.Error

-- temporary:
import Utils.Debug

typeCheck :: Program -> Err Env
typeCheck prog = do
    env  <- allFunctions prog
    env1 <- mainExists env
    env2 <- checkProg env1 prog
    env3 <- returnCheck env2 prog
    return env3

typeCheck' :: Program -> Eval Program
typeCheck' prog = do
    allFunctions' prog
    return prog
    --env  <- allFunctions prog
    --env1 <- mainExists env
    --env2 <- checkProg env1 prog
    --env3 <- returnCheck env2 prog
    -- return env3

allFunctions' :: Program -> Eval ()
allFunctions' = undefined -- collectFuns' . (predefFuns' ++) . progFunSigs'

collectFuns' :: [FunId] -> Eval ()
collectFuns' = undefined --foldM (extendFun' $ err

predefFuns' :: [FunId]
predefFuns' = map toFunId
             [("printInt",    ([Int],      Void)),
              ("printDouble", ([Doub],     Void)),
              ("printString", ([ConstStr], Void)),
              ("readInt",     ([],         Int)),
              ("readDouble",  ([],         Doub))]

progFunSigs' :: Program -> [FunId]
progFunSigs' = map toFnSigId' . progFuns

allFunctions :: Program -> Err Env
allFunctions = collectFuns emptyEnv . (predefFuns ++) . progFunSigs

mainId :: Ident
mainId = Ident "main"

mainExists :: Env -> Err Env
mainExists env = do
    (args, rtype) <- lookupFun env mainId
    case rtype == Int && null args of
        True  -> return env
        False -> Left $ wrgFunSig mainId

predefFuns :: [FunId]
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
toFnSigId (FnDef ret ident args _) = (ident, (map argType args, ret))

extendFun :: Env -> FnSigId -> Err Env
extendFun (sigs, ctxs) (ident, sig) =
    case Map.lookup ident sigs of
    Nothing -> Right (Map.insert ident sig sigs, ctxs)
    Just _  -> Left $ funcAlrDef ident

checkProg :: Env -> Program -> Err Env
checkProg env = foldM checkFun env . progFuns

emptyEnv :: Env
emptyEnv = (Map.empty, [Map.empty])

newBlock :: Env -> Env
newBlock (sigs, ctxs)  = (sigs, Map.empty:ctxs)

remBlock :: Env -> Env
remBlock (sigs, []) = (sigs, [])
remBlock (sigs, ctxs)  = (sigs, tail ctxs)

lookupVar :: Env -> Ident -> Err Type
lookupVar (_, contexts) ident = maybe (Left $ varNotDef ident) return $
                                      (mfind . Map.lookup) ident contexts

extendVar :: Env -> Ident -> Type -> Err Env
extendVar (_, []) _ _ = Left ""
extendVar (sigs, c:cs) ident typ = case Map.lookup ident c of
    Nothing -> Right (sigs, Map.insert ident typ c:cs)
    Just _  -> Left $ varAlrDef ident

lookupFun :: Env -> Ident -> Err FnSig
lookupFun (sigs, _) ident = case Map.lookup ident sigs of
        Just sig -> return sig
        Nothing  -> Left $ funcNotDef ident

inferExp :: Env -> Expr -> Err Type
inferExp env expr = case expr of
    EVar ident   -> lookupVar env ident
    ELitInt _    -> return Int
    ELitDoub _   -> return Doub
    ELitTrue     -> return Bool
    ELitFalse    -> return Bool
    EApp ide xps -> inferFun env ide xps
    EString _    -> return ConstStr
    Neg xpr      -> inferUnary env [Int, Doub] xpr
    Not xpr      -> inferUnary env [Bool] xpr
    EMul le o ri -> inferBinary env (mulOp o) le ri
    EAdd le _ ri -> inferBinary env [Int, Doub] le ri
    ERel le o ri -> inferBinary env (relOp o) le ri >> return Bool
    EAnd le ri   -> inferBinary env [Bool] le ri
    EOr le ri    -> inferBinary env [Bool] le ri

relOp :: RelOp -> [Type]
relOp oper | oper `elem` [NE, EQU] = [Int, Doub, Bool]
           | otherwise             = [Int, Doub]

mulOp :: MulOp -> [Type]
mulOp oper | oper == Mod = [Int]
           | otherwise   = [Int, Doub]

inferBinary :: Env -> [Type] -> Expr -> Expr -> Err Type
inferBinary env types exp1 exp2 = do
    typl <- inferExp env exp1
    typr <- inferExp env exp2
    case typl == typr && typl `elem` types of
        True  -> return typl
        False -> Left $ wrgBinExp exp1 exp2 typl typr

inferUnary :: Env -> [Type] -> Expr -> Err Type
inferUnary env types expr = do
    typ <- inferExp env expr
    case typ `elem` types of
        True  -> return typ
        False -> Left $ wrgUnaExp expr types typ

inferFun :: Env -> Ident -> [Expr] -> Err Type
inferFun env ident exprs = do
    (argtypes, rtype) <- lookupFun env ident
    exprtypes         <- mapM (inferExp env) exprs
    case argtypes == exprtypes of
        True  -> return rtype
        False -> Left $ wrgArgTyp ident argtypes exprtypes

checkFun :: Env -> TopDef -> Err Env
checkFun env (FnDef rtype _ args block) = do
    env2 <- foldM (\en (Arg typ aident) ->
                   extendVar en aident typ) (newBlock env) args
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
    Ass ident expr      -> checkIdeExp env ident expr >> return env
    Incr ident          -> checkIdent env [Int, Doub] ident >> return env
    Decr ident          -> checkIdent env [Int, Doub] ident >> return env
    Ret expr            -> checkExp env typ expr >> return env
    VRet                -> checkVoid typ >> return env
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

checkIdeExp :: Env -> Ident -> Expr -> Err Type
checkIdeExp env ident expr = do
    typ <- lookupVar env ident
    checkExp env typ expr

checkIdent :: Env -> [Type] -> Ident -> Err Type
checkIdent env types ident = do
    typ <- lookupVar env ident
    case typ `elem` types of
        True  -> return typ
        False -> Left $ wrgIdeTyp ident types typ

checkVoid :: Type -> Err Type
checkVoid typ
    | typ == Void = return typ
    | otherwise   = Left $ wrgVoidTyp typ

checkDecl :: Type -> Env -> Item -> Err Env
checkDecl typ env item = do
    ident <- case item of
             Init ident expr -> checkExp env typ expr >> return ident
             NoInit ident    -> return ident
    extendVar env ident typ
