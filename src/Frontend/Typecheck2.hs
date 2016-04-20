{- Javalette Compiler
   Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -}

{-|
Module      : Frontend.Typecheck
Description : Type checker for Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
Stability   : experimental
Portability : ALL

Type checker for Javalette compiler.
-}
{-# LANGUAGE TupleSections #-}

module Frontend.Typecheck2 (
    -- * Operations
    typeCheck
) where

import Control.Monad
import Control.Arrow

import Javalette.Abs

import Frontend.Types
import Frontend.Query
import Frontend.Error2

import Frontend.ReturnCheck

-- temporary:
import Utils.Debug

--------------------------------------------------------------------------------
-- API:
--------------------------------------------------------------------------------

typeCheck :: Program -> Eval Program
typeCheck prog = do
    -- P1: collect functions:
    allFunctions prog
    -- P2: check for existance + correctness of main definition:
    mainCorrect
    -- P3: type check the program
    prog <- checkProg prog
    -- P4: return check the program.
    -- prog' <- returnCheck prog
    let prog' = prog
    return prog'

--------------------------------------------------------------------------------
-- Checking for int main(void):
--------------------------------------------------------------------------------

mainId :: Ident
mainId = Ident "main"

mainCorrect :: Eval ()
mainCorrect = lookupFunE mainId >>= flip unless wrongMainSig . (== FunSig [] Int)

--------------------------------------------------------------------------------
-- Collecting function signatures:
--------------------------------------------------------------------------------

allFunctions :: Program -> Eval ()
allFunctions = collectFuns . (predefFuns ++) . extractFunIds

collectFuns :: [FunId] -> Eval ()
collectFuns = mapM_ $ extendFun' funAlreadyDef

predefFuns :: [FunId]
predefFuns = map toFunId
    [("printInt",    ([Int],      Void)),
     ("printDouble", ([Doub],     Void)),
     ("printString", ([ConstStr], Void)),
     ("readInt",     ([],         Int)),
     ("readDouble",  ([],         Doub))]

extractFunIds :: Program -> [FunId]
extractFunIds = map toFnSigId . progFuns

toFnSigId :: TopDef -> FunId
toFnSigId (FnDef ret ident args _) = FunId ident $ FunSig (map argType args) ret

--------------------------------------------------------------------------------
-- Type checking:
--------------------------------------------------------------------------------

checkProg :: Program -> Eval Program
checkProg = fmap Program . mapM checkFun . progFuns

checkFun :: TopDef -> Eval TopDef
checkFun (FnDef rtype ident args block) = do
    pushBlock
    collectArgVars args
    FnDef rtype ident args <$> checkBlock rtype block

collectArgVars :: [Arg] -> Eval ()
collectArgVars = mapM_ $ extendVar' argAlreadyDef . argToVar

checkBlock :: Type -> Block -> Eval Block
checkBlock frtyp (Block block) = Block <$> checkStms frtyp block <* popBlock

checkStms :: Type -> [Stmt] -> Eval [Stmt]
checkStms = mapM . checkStm

(<<$>) :: Functor f => (a -> c) -> f (a, b) -> f c
f <<$> y = f . fst <$> y

checkStm :: Type -> Stmt -> Eval Stmt
checkStm typ stmt = case stmt of
    Empty               -> return stmt
    BStmt block         -> pushBlock >> BStmt <$> checkBlock typ block
    Decl vtyp items     -> Decl vtyp <$> checkDecls vtyp items
    Ass ident expr      -> Ass ident <$> checkIdentExp ident expr
    Incr ident          -> Incr <$> checkIdent [Int, Doub] ident
    Decr ident          -> Decr <$> checkIdent [Int, Doub] ident
    SExp expr           -> SExp <<$> inferExp expr
    Ret expr            -> Ret <$> checkExp typ expr
    VRet                -> checkVoid typ >> return VRet
    While expr st       -> checkC While    expr st
    Cond expr st        -> checkC Cond     expr st
    CondElse expr si se -> checkC CondElse expr si <*> checkR se
    where checkR = checkStm typ
          checkC ctor expr st = ctor <$> checkExp Bool expr <*> checkR st

checkVoid :: Type -> Eval ()
checkVoid frtyp = unless (frtyp == Void) $ wrongRetTyp Void frtyp

checkIdentExp :: Ident -> Expr -> Eval Expr
checkIdentExp ident expr = lookupVarE ident >>= flip checkExp expr

checkDecls :: Type -> [Item] -> Eval [Item]
checkDecls vtyp = mapM single
    where single item = checkDeclItem vtyp item <*
                        extendVar' varAlreadyDef (itemToVar vtyp item)

checkDeclItem :: Type -> Item -> Eval Item
checkDeclItem vtyp (Init ident expr)   = Init ident <$> checkExp vtyp expr
checkDeclItem vtyp item@(NoInit ident) = return item

checkExp :: Type -> Expr -> Eval Expr
checkExp texpected expr = do
    (expr', tactual) <- inferExp expr
    if texpected == tactual then return expr'
                            else wrongExpTyp expr texpected tactual

checkIdent :: [Type] -> Ident -> Eval Ident
checkIdent types ident = do
    vtyp <- lookupVarE ident
    if vtyp `elem` types then return ident
                         else wrongIdentTyp ident types vtyp

--------------------------------------------------------------------------------
-- Type inference:
--------------------------------------------------------------------------------

inferExp :: Expr -> Eval (Expr, Type)
inferExp expr = case expr of
    EVar ident       -> (EVar ident,) <$> lookupVarE ident
    EString  v       -> return (EString  v, ConstStr)
    ELitInt  v       -> return (ELitInt  v, Int     )
    ELitDoub v       -> return (ELitDoub v, Doub    )
    ELitTrue         -> return (ELitTrue  , Bool    )
    ELitFalse        -> return (ELitFalse , Bool    )
    EApp ident exprs -> first (EApp ident) <$> inferFun ident exprs
    Neg expr         -> inferUnary [Int, Doub] expr
    Not expr         -> inferUnary [Bool] expr
    EMul le oper re  -> emul oper <$> inferBinary (mulOp oper) le re
    EAdd le oper re  -> eadd oper <$> inferBinary [Int, Doub]  le re
    ERel le oper re  -> erel oper <$> inferBinary (relOp oper) le re
    EAnd le re       -> eand      <$> inferBinary [Bool] le re
    EOr  le re       -> eor       <$> inferBinary [Bool] le re

emul = mergeBin . flip EMul
eadd = mergeBin . flip EAdd
erel = mergeBin . flip ERel
eand = mergeBin        EAnd
eor  = mergeBin        EOr

relOp :: RelOp -> [Type]
relOp oper | oper `elem` [NE, EQU] = [Int, Doub, Bool]
           | otherwise             = [Int, Doub]

mulOp :: MulOp -> [Type]
mulOp oper | oper == Mod = [Int]
           | otherwise   = [Int, Doub]

mergeBin :: (le -> re -> e) -> (le, re, rt) -> (e, rt)
mergeBin op (l, r, etyp) = (l `op` r, etyp)

inferBinary :: [Type] -> Expr -> Expr -> Eval (Expr, Expr, Type)
inferBinary types exprl exprr = do
    (exprl', typl) <- inferExp exprl
    (exprr', typr) <- inferExp exprr
    if typl == typr && typl `elem` types then return (exprl', exprr', typl)
                                         else wrongBinExp exprl exprr typl typr

inferUnary :: [Type] -> Expr -> Eval (Expr, Type)
inferUnary types expr = do
    r@(expr', etyp) <- inferExp expr
    if etyp `elem` types then return r
                         else wrongUnaryExp expr types etyp

inferFun :: Ident -> [Expr] -> Eval ([Expr], Type)
inferFun ident exprs = do
    FunSig texpected rtype <- lookupFunE ident
    (exprs', tactual)      <- mapAndUnzipM inferExp exprs
    if texpected == tactual then return (exprs', rtype)
                            else wrongArgsTyp ident texpected tactual

--------------------------------------------------------------------------------
-- Lookups:
--------------------------------------------------------------------------------
lookupFunE = lookupFun' funNotDef
lookupVarE = lookupVar' varNotDef