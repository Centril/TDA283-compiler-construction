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
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase, TypeSynonymInstances,
             FlexibleInstances, UndecidableInstances #-}

module Frontend.TypeCheck (
    -- * Modules
    module X,

    -- * Operations
    typeCheck, compileFrontend, targetTypeCheck
) where

import Control.Monad
import Control.Monad.Reader

import Control.Lens hiding (contexts, Empty)

import Utils.Monad
import Utils.Sizeables

import Frontend.Environment as X
import Frontend.Error
import Frontend.Common
import Frontend.TypeFunSig
import Frontend.TypeInfer
import Frontend.ReturnCheck
import Frontend.ParseLex

import Common.AST
import Common.FileOps

--------------------------------------------------------------------------------
-- TARGET, Typecheck:
--------------------------------------------------------------------------------

targetTypeCheck :: JlcTarget
targetTypeCheck = flip (evalIOComp compileTCIO) initialTCEnv

compileTCIO :: IOTCComp ()
compileTCIO = jlFiles . classifyInputs <$> ask
              >>= mapM_ (readF >=> rebase . compileFrontend)

--------------------------------------------------------------------------------
-- API:
--------------------------------------------------------------------------------

compileFrontend :: String -> TCComp ProgramA
compileFrontend code = do
    ast1 <- parseProgram code <<= infoP Parser "AST after parse"
    typeCheck ast1            <<= phaseEnd TypeChecker

typeCheck :: Program () -> TCComp ProgramA
typeCheck prog0 = do
    let prog1 = fmap (const emptyAnot) prog0
    -- Part 1: Collect all functions
    info TypeChecker   "Collecting all functions"
    prog2 <- allFunctions prog1
    info TypeChecker   "AST after collect"
    info TypeChecker $ show prog2
    -- Part 2: Check for the correctness of the main definition
    info TypeChecker   "Checking existence of main"
    mainCorrect
    -- Part 3: Type check the program
    info TypeChecker   "Type checking the program"
    prog3 <- checkProg prog2
    -- Part 4: Return check the program.
    info ReturnChecker "Return checking the program"
    returnCheck prog3

--------------------------------------------------------------------------------
-- Type checking:
--------------------------------------------------------------------------------

checkProg :: ProgramA -> TCComp ProgramA
checkProg = pTopDefs %%~ mapM checkFunType

checkFunType :: TopDefA -> TCComp TopDefA
checkFunType fun = sPushM contexts >> collectArgVars (_fArgs fun) >>
                   (fBlock %%~ checkBlock (_fRetTyp fun) $ fun)

collectArgVars :: [ArgA] -> TCComp ()
collectArgVars = mapM_ $ extendVar' argAlreadyDef . argToVar

checkBlock :: TypeA -> BlockA -> TCComp BlockA
checkBlock rtyp block = (bStmts %%~ mapM (checkStm rtyp) $ block) <*
                        checkUnused <* sPopM contexts

checkUnused :: TCComp ()
checkUnused = view (compileFlags . noWarnUnused) >>=
              flip unless (currUnused >>= mapM_ unusedVar)

checkStm :: TypeA -> StmtA -> TCComp StmtA
checkStm typ stmt = case stmt of
    Empty     _ -> return stmt
    BStmt    {} -> sPushM contexts >> (sBlock %%~ checkBlock typ $ stmt)
    Decl     {} -> checkDecls stmt
    Ass      {} -> checkAss stmt
    AssArr   {} -> checkAssArr stmt
    Incr     {} -> checkInc
    Decr     {} -> checkInc
    SExp     {} -> sExpr %%~ fmap fst . inferExp $ stmt
    Ret      {} -> sExpr %%~ checkExp typ $ stmt
    VRet      _ -> checkVoid typ >> return stmt
    While    {} -> checkC stmt
    Cond     {} -> checkC stmt
    CondElse {} -> checkC >=> checkS sSe $ stmt
    For      {} -> checkFor typ stmt
    where checkC   = sExpr %%~ checkExp bool >=> checkS sSi
          checkS f = f %%~ checkStm typ
          checkInc = checkIdent [int, doub] stmt

instance Growable TypeA where
    grow = \case
        Array _ b dts -> arrayT b $ 1 + length dts
        x             -> arrayT x 1

arrayT :: TypeA -> Int -> TypeA
arrayT base dim = appConcrete make
    where make a = Array a base $ replicate dim $ DimenT emptyAnot

checkFor :: TypeA -> StmtA -> TCComp StmtA
checkFor rtyp for =
    ((sExpr %%~ checkExp (grow typ) $ for) <* sPushM contexts
    <* (extendVar' varAlreadyDef $ Var (_sIdent for) typ VSLocal 0)
    >>= (sSi %%~ checkStm rtyp))           <* sPopM contexts
    where typ = _sTyp for

checkAss :: StmtA -> TCComp StmtA
checkAss ass = do
    (ass', typ) <- lookupVarE' ( _sIdent ass) ass
    sExpr %%~ checkExp typ $ ass'

-- TODO: Implement
checkAssArr :: StmtA -> TCComp StmtA
checkAssArr = error "checkAssArr undefined"

checkVoid :: TypeA -> TCComp ()
checkVoid frtyp = unless (frtyp == tvoid) (wrongRetTyp tvoid frtyp)

checkDecls :: StmtA -> TCComp StmtA
checkDecls decl = do
    (vtyp', _) <- inferType $ _sDTyp decl
    sDItems %%~ mapM (single vtyp') $ decl { _sDTyp = vtyp' }
    where single vt item = checkDeclItem vt item <*
                           extendVar' varAlreadyDef (itemToVar vt item)

checkDeclItem :: TypeA -> ItemA -> TCComp ItemA
checkDeclItem _    item@(NoInit _ _) = return item
checkDeclItem vtyp item              = iExpr %%~ checkExp vtyp $ item

checkExp :: TypeA -> ExprA -> TCComp ExprA
checkExp texpected expr = fst <$> unless' (inferExp expr) ((texpected ==) . snd)
                                  (wrongExpTyp expr texpected . snd)

checkIdent :: [TypeA] -> StmtA -> TCComp StmtA
checkIdent types stmt = do
    let name = _sIdent stmt
    (stmt', typ) <- lookupVarE' name stmt
    fst . addTyp stmt' <$> unless' (return typ) (`elem` types)
                                   (wrongIdentTyp name types)

argToVar :: ArgA -> Var
argToVar a = Var (_aIdent a) (_aTyp a) VSArg 0

itemToVar :: TypeA -> ItemA -> Var
itemToVar typ item = Var (_iIdent item) typ VSLocal 0