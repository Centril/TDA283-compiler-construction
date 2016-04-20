{- Javalette Compiler, a simple C like language.
 - Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -
 - This program is free software; you can redistribute it and/or
 - modify it under the terms of the GNU General Public License
 - as published by the Free Software Foundation; either version 2
 - of the License, or (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program; if not, write to the Free Software
 - Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 -}

{-|
Module      : Frontend.Typecheck
Description : Type checker for Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Type checker for Javalette compiler.
-}
{-# LANGUAGE TupleSections #-}

module Frontend.TypeCheck (
    -- * Operations
    typeCheck
) where

import Control.Monad
import Control.Arrow

import Javalette.Abs

import Frontend.Types
import Frontend.Query
import Frontend.Error

import Frontend.ReturnCheck

import Utils.Monad

--------------------------------------------------------------------------------
-- API:
--------------------------------------------------------------------------------

typeCheck :: Program () -> Eval (Program ())
typeCheck prog1 = do
    -- P1: collect functions:
    info TypeChecker   "Collecting all functions"
    allFunctions prog1
    -- P2: check for existance + correctness of main definition:
    info TypeChecker   "Checking existence of main"
    mainCorrect
    -- P3: type check the program
    info TypeChecker   "Type checking the program"
    prog2 <- checkProg prog1
    -- P4: return check the program.
    info ReturnChecker "Return checking the program"
    returnCheck prog2

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

allFunctions :: Program a -> Eval ()
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

extractFunIds :: Program a -> [FunId]
extractFunIds = map toFnSigId . progFuns

toFnSigId :: TopDef a -> FunId
toFnSigId (FnDef a ret ident args _) = FunId ident $ FunSig (map argType args) ret

--------------------------------------------------------------------------------
-- Type checking:
--------------------------------------------------------------------------------

checkProg :: Program a -> Eval (Program a)
checkProg = fmap Program . mapM checkFun . progFuns

checkFun :: TopDef a -> Eval (TopDef a)
checkFun (FnDef rtype ident args block) = do
    pushBlock
    collectArgVars args
    FnDef rtype ident args <$> checkBlock rtype block

collectArgVars :: [Arg a] -> Eval ()
collectArgVars = mapM_ $ extendVar' argAlreadyDef . argToVar

checkBlock :: Type a -> Block a -> Eval (Block a)
checkBlock frtyp (Block block) = Block <$> checkStms frtyp block <* popBlock

checkStms :: Type a -> [Stmt a] -> Eval [Stmt a]
checkStms = mapM . checkStm

checkStm :: Type a -> Stmt a -> Eval (Stmt a)
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

checkVoid :: Type a -> Eval ()
checkVoid frtyp = unless (frtyp == Void) $ wrongRetTyp Void frtyp

checkIdentExp :: Ident -> Expr a -> Eval (Expr a)
checkIdentExp ident expr = lookupVarE ident >>= flip checkExp expr

checkDecls :: Type a -> [Item a] -> Eval [Item a]
checkDecls vtyp = mapM single
    where single item = checkDeclItem vtyp item <*
                        extendVar' varAlreadyDef (itemToVar vtyp item)

checkDeclItem :: Type a -> Item a -> Eval (Item a)
checkDeclItem vtyp (Init ident expr) = Init ident <$> checkExp vtyp expr
checkDeclItem _    item@(NoInit _)   = return item

checkExp :: Type a -> Expr a -> Eval (Expr a)
checkExp texpected expr = do
    (expr', tactual) <- inferExp expr
    if texpected == tactual then return expr'
                            else wrongExpTyp expr texpected tactual

checkIdent :: [Type a] -> Ident -> Eval Ident
checkIdent types ident = do
    vtyp <- lookupVarE ident
    if vtyp `elem` types then return ident
                         else wrongIdentTyp ident types vtyp

--------------------------------------------------------------------------------
-- Type inference:
--------------------------------------------------------------------------------

inferExp :: Expr a -> Eval (Expr a, Type a)
inferExp expr = case expr of
    EVar ident   -> (EVar ident,) <$> lookupVarE ident
    EString  v   -> return (EString  v, ConstStr)
    ELitInt  v   -> return (ELitInt  v, Int     )
    ELitDoub v   -> return (ELitDoub v, Doub    )
    ELitTrue     -> return (ELitTrue  , Bool    )
    ELitFalse    -> return (ELitFalse , Bool    )
    EApp ident e -> first (EApp ident) <$> inferFun ident e
    Neg e        -> inferUnary [Int, Doub] e
    Not e        -> inferUnary [Bool] e
    EMul l op r  -> inferBin (`EMul` op) (mulOp op)  l r
    EAdd l op r  -> inferBin (`EAdd` op) [Int, Doub] l r
    ERel l op r  -> second (const Bool) <$> inferBin (`ERel` op) (relOp op) l r
    EAnd l r     -> inferBin EAnd        [Bool]      l r
    EOr  l r     -> inferBin EOr         [Bool]      l r

inferBin :: (Expr a -> Expr a -> Expr a)
         -> [Type a] -> Expr a -> Expr a -> Eval (Expr a, Type a)
inferBin op accept le re = first (uncurry op) <$> inferBinary accept le re

relOp :: RelOp a -> [Type a]
relOp oper | oper `elem` [NE, EQU] = [Int, Doub, Bool]
           | otherwise             = [Int, Doub]

mulOp :: MulOp a -> [Type a]
mulOp oper | oper == Mod = [Int]
           | otherwise   = [Int, Doub]

inferBinary :: [Type a] -> Expr a -> Expr a -> Eval ((Expr a, Expr a), Type a)
inferBinary types exprl exprr = do
    (exprl', typl) <- inferExp exprl
    (exprr', typr) <- inferExp exprr
    if typl == typr && typl `elem` types then return ((exprl', exprr'), typl)
                                         else wrongBinExp exprl exprr typl typr

inferUnary :: [Type a] -> Expr a -> Eval (Expr a, Type a)
inferUnary types expr = do
    r@(_, etyp) <- inferExp expr
    if etyp `elem` types then return r
                         else wrongUnaryExp expr types etyp

inferFun :: Ident -> [Expr a] -> Eval ([Expr a], Type a)
inferFun ident exprs = do
    FunSig texpected rtype <- lookupFunE ident
    (exprs', tactual)      <- mapAndUnzipM inferExp exprs
    if texpected == tactual then return (exprs', rtype)
                            else wrongArgsTyp ident texpected tactual

--------------------------------------------------------------------------------
-- Lookups:
--------------------------------------------------------------------------------

lookupFunE :: Ident -> Eval FunSig
lookupFunE = lookupFun' funNotDef

lookupVarE :: Ident -> Eval (Type ())
lookupVarE = lookupVar' varNotDef