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

makeBin f a = flip (f a)
applyEA = fmap ($ emptyAnot)

--------------------------------------------------------------------------------
-- API:
--------------------------------------------------------------------------------

typeCheck :: Program () -> Eval (Program ASTAnots)
typeCheck prog0 = do
    let prog1 = fmap (const emptyAnot) prog0
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
mainCorrect = lookupFunE mainId >>=
    flip unless wrongMainSig . (== FunSig [] (Int emptyAnot))

--------------------------------------------------------------------------------
-- Collecting function signatures:
--------------------------------------------------------------------------------

allFunctions :: Program ASTAnots -> Eval ()
allFunctions = collectFuns . (predefFuns ++) . extractFunIds

collectFuns :: [FunId] -> Eval ()
collectFuns = mapM_ $ extendFun' funAlreadyDef

predefFuns :: [FunId]
predefFuns = map toFunId
    [("printInt",    ([Int     ], Void)),
     ("printDouble", ([Doub    ], Void)),
     ("printString", ([ConstStr], Void)),
     ("readInt",     ([        ], Int )),
     ("readDouble",  ([        ], Doub))]

extractFunIds :: Program ASTAnots -> [FunId]
extractFunIds = map toFnSigId . progFuns

toFnSigId :: TopDef ASTAnots -> FunId
toFnSigId (FnDef _ ret ident args _) =
    FunId ident $ FunSig (map argType args) ret

--------------------------------------------------------------------------------
-- Type checking:
--------------------------------------------------------------------------------

checkProg :: Program ASTAnots -> Eval (Program ASTAnots)
checkProg (Program a funs) = Program a <$> mapM checkFunType funs

checkFunType :: TopDef ASTAnots -> Eval (TopDef ASTAnots)
checkFunType (FnDef a rtype ident args block) = do
    pushBlock
    collectArgVars args
    FnDef a rtype ident args <$> checkBlock rtype block

collectArgVars :: [Arg ASTAnots] -> Eval ()
collectArgVars = mapM_ $ extendVar' argAlreadyDef . argToVar

checkBlock :: Type ASTAnots -> Block ASTAnots -> Eval (Block ASTAnots)
checkBlock frtyp (Block a block) =
    Block a <$> checkStms frtyp block <* popBlock

checkStms :: Type ASTAnots -> [Stmt ASTAnots] -> Eval [Stmt ASTAnots]
checkStms = mapM . checkStm

checkStm :: Type ASTAnots -> Stmt ASTAnots -> Eval (Stmt ASTAnots)
checkStm typ stmt = case stmt of
    Empty _               -> return stmt
    BStmt a block         -> pushBlock >> BStmt a <$> checkBlock typ block
    Decl a vtyp items     -> Decl a vtyp <$> checkDecls vtyp items
    Ass a ident expr      -> Ass a ident <$> checkIdentExp ident expr
    Incr a ident          -> Incr a <$> checkIdent [Int a, Doub a] ident
    Decr a ident          -> Decr a <$> checkIdent [Int a, Doub a] ident
    SExp a expr           -> SExp a <<$> inferExp expr
    Ret a expr            -> Ret a <$> checkExp typ expr
    VRet a                -> checkVoid typ >> return (VRet a)
    While a expr st       -> checkC (While a) a expr st
    Cond a expr st        -> checkC (Cond a) a expr st
    CondElse a expr si se -> checkC (CondElse a) a expr si <*> checkR se
    where checkR = checkStm typ
          checkC ctor a expr st =
              ctor <$> checkExp (Bool a) expr <*> checkR st

checkVoid :: Type ASTAnots -> Eval ()
checkVoid frtyp = unless (void frtyp == Void ()) $
    wrongRetTyp (Void ()) $ void frtyp

checkIdentExp :: Ident -> Expr ASTAnots -> Eval (Expr ASTAnots)
checkIdentExp ident expr = lookupVarE ident >>= flip checkExp expr

checkDecls :: Type ASTAnots -> [Item ASTAnots] -> Eval [Item ASTAnots]
checkDecls vtyp = mapM single
    where single item = checkDeclItem vtyp item <*
                        extendVar' varAlreadyDef (itemToVar vtyp item)

checkDeclItem :: Type ASTAnots -> Item ASTAnots -> Eval (Item ASTAnots)
checkDeclItem vtyp (Init a ident expr) = Init a ident <$> checkExp vtyp expr
checkDeclItem _    item@(NoInit a _)   = return item

checkExp :: Type ASTAnots -> Expr ASTAnots -> Eval (Expr ASTAnots)
checkExp texpected expr = do
    (expr', tactual) <- inferExp expr
    if void texpected == void tactual
        then return expr'
        else wrongExpTyp (void expr) (void texpected) (void tactual)

checkIdent :: [Type ASTAnots] -> Ident -> Eval Ident
checkIdent types ident = do
    vtyp <- lookupVarE ident
    if (void vtyp) `elem` (void <$> types)
        then return ident
        else wrongIdentTyp ident (void <$> types) (void vtyp)

--------------------------------------------------------------------------------
-- Type inference:
--------------------------------------------------------------------------------

inferExp :: Expr ASTAnots -> Eval (Expr ASTAnots, Type ASTAnots)
inferExp expr = case expr of
    EVar a ident   -> (EVar a ident,) <$> lookupVarE ident
    EString a  v   -> return (EString a v, ConstStr a)
    ELitInt a v    -> return (ELitInt a v, Int a     )
    ELitDoub a v   -> return (ELitDoub a v, Doub a   )
    ELitTrue a     -> return (ELitTrue a  , Bool a   )
    ELitFalse a    -> return (ELitFalse a , Bool a   )
    EApp a ident e -> first (EApp a ident) <$> inferFun ident e
    Neg a e        -> inferUnary [Int a, Doub a] e
    Not a e        -> inferUnary [Bool a] e
    EMul a l op r  -> inferBin (makeBin EMul a op) (mulOp op)  l r
    EAdd a l op r  -> inferBin (makeBin EAdd a op) [Int a, Doub a] l r
    ERel a l op r  -> second (const $ Bool a) <$>
                      inferBin (makeBin ERel a op) (relOp op) l r
    EAnd a l r     -> inferBin (EAnd a)    [Bool a]    l r
    EOr  a l r     -> inferBin (EOr a)     [Bool a]    l r

inferBin :: (Expr ASTAnots -> Expr ASTAnots -> Expr ASTAnots)
         -> [Type ASTAnots]
         -> Expr ASTAnots
         -> Expr ASTAnots
         -> Eval (Expr ASTAnots, Type ASTAnots)
inferBin op accept le re = first (uncurry op) <$> inferBinary accept le re

relOp :: RelOp ASTAnots -> [Type ASTAnots]
relOp oper | oper `elem` applyEA [NE, EQU] = applyEA [Int, Doub, Bool]
           | otherwise                     = applyEA [Int, Doub]

mulOp :: MulOp ASTAnots -> [Type ASTAnots]
mulOp oper | oper == Mod emptyAnot = applyEA [Int]
           | otherwise             = applyEA [Int, Doub]

inferBinary :: [Type ASTAnots] -> Expr ASTAnots -> Expr ASTAnots -> Eval ((Expr ASTAnots, Expr ASTAnots), Type ASTAnots)
inferBinary types exprl exprr = do
    (exprl', typl) <- inferExp exprl
    (exprr', typr) <- inferExp exprr
    if typl == typr && typl `elem` types
        then return ((exprl', exprr'), typl)
        else wrongBinExp exprl exprr typl typr

inferUnary :: [Type ASTAnots] -> Expr ASTAnots -> Eval (Expr ASTAnots, Type ASTAnots)
inferUnary types expr = do
    r@(_, etyp) <- inferExp expr
    if etyp `elem` types
        then return r
        else wrongUnaryExp expr types etyp

inferFun :: Ident -> [Expr ASTAnots] -> Eval ([Expr ASTAnots], Type ASTAnots)
inferFun ident exprs = do
    FunSig texpected rtype <- lookupFunE ident
    (exprs', tactual)      <- mapAndUnzipM inferExp exprs
    if texpected == tactual
        then return (exprs', rtype)
        else wrongArgsTyp ident texpected tactual

--------------------------------------------------------------------------------
-- Lookups:
--------------------------------------------------------------------------------

lookupFunE :: Ident -> Eval FunSig
lookupFunE = lookupFun' funNotDef

lookupVarE :: Ident -> Eval (Type ASTAnots)
lookupVarE = lookupVar' varNotDef