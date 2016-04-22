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
module Frontend.TypeCheck (
    -- * Operations
    typeCheck
) where

import Control.Monad

import Javalette.Abs

import Frontend.Annotations
import Frontend.Computation
import Frontend.Query
import Frontend.Error
import Frontend.Common
import Frontend.TypeFunSig
import Frontend.TypeInfer
import Frontend.ReturnCheck

import Utils.Monad

--------------------------------------------------------------------------------
-- API:
--------------------------------------------------------------------------------

typeCheck :: Program () -> Eval ProgramA
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
-- Type checking:
--------------------------------------------------------------------------------

checkProg :: ProgramA -> Eval ProgramA
checkProg (Program a funs) = Program a <$> mapM checkFunType funs

checkFunType :: TopDefA -> Eval TopDefA
checkFunType (FnDef a rtype ident args block) = do
    pushBlock
    collectArgVars args
    FnDef a rtype ident args <$> checkBlock rtype block

collectArgVars :: [ArgA] -> Eval ()
collectArgVars = mapM_ $ extendVar' argAlreadyDef . argToVar

checkBlock :: TypeA -> BlockA -> Eval BlockA
checkBlock frtyp (Block a block) =
    Block a <$> checkStms frtyp block <* popBlock

checkStms :: TypeA -> [StmtA] -> Eval [StmtA]
checkStms = mapM . checkStm

checkStm :: TypeA -> StmtA -> Eval StmtA
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

checkVoid :: TypeA -> Eval ()
checkVoid frtyp = unless (void frtyp == Void ()) $
    wrongRetTyp (Void ()) $ void frtyp

checkIdentExp :: Ident -> ExprA -> Eval ExprA
checkIdentExp ident expr = lookupVarE ident >>= flip checkExp expr

checkDecls :: TypeA -> [ItemA] -> Eval [ItemA]
checkDecls vtyp = mapM single
    where single item = checkDeclItem vtyp item <*
                        extendVar' varAlreadyDef (itemToVar vtyp item)

checkDeclItem :: TypeA -> ItemA -> Eval ItemA
checkDeclItem vtyp (Init a ident expr) = Init a ident <$> checkExp vtyp expr
checkDeclItem _    item@(NoInit _ _)   = return item

checkExp :: TypeA -> ExprA -> Eval ExprA
checkExp texpected expr = do
    (expr', tactual) <- inferExp expr
    if texpected == tactual
        then return expr'
        else wrongExpTyp expr texpected tactual

checkIdent :: [TypeA] -> Ident -> Eval Ident
checkIdent types ident = do
    vtyp <- lookupVarE ident
    if vtyp `elem` types
        then return ident
        else wrongIdentTyp ident types vtyp