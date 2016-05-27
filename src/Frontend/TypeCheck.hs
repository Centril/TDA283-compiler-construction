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
{-# LANGUAGE LambdaCase #-}

module Frontend.TypeCheck (
    -- * Modules
    module X,

    -- * Operations
    typeCheck, compileFrontend, targetTypeCheck
) where

import Data.Data (Data)

import Control.Monad
import Control.Monad.Reader

import Control.Lens hiding (contexts, Empty)

import qualified Data.Generics.Uniplate.Data as U

import Utils.Pointless
import Utils.Foldable
import Utils.Monad
import Utils.Sizeables

import Frontend.Environment as X
import Frontend.Error
import Frontend.TypeFunSig
import Frontend.TypeInfer
import Frontend.ReturnCheck
import Frontend.ParseLex

import Common.AST
import Common.ASTOps
import Common.FileOps

u = undefined

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
    -- Part 1: Collect all typedefs
    info TypeChecker   "Collecting typedefs"
    collectTypedefs prog1
    -- Part 2: Collect all structs + classes
    info TypeChecker   "Collecting structs and classes"
    collectComplex prog1
    -- Part 3: Expand typedefs
    info TypeChecker   "Expanding typedefs"
    prog2 <- expandTypedefs >=> nukeTypedefs $ prog1
    -- Part 4: Collect all functions
    info TypeChecker   "Collecting all functions"
    prog3 <- allFunctions prog2
    info TypeChecker   "AST after collect"
    info TypeChecker $ show prog3
    -- Part 5: Check for the correctness of the main definition
    info TypeChecker   "Checking existence of int main()"
    mainCorrect
    -- Part 6: Type check the program
    info TypeChecker   "Type checking the program"
    prog4 <- checkProg prog3
    -- Part 7: Return check the program.
    info ReturnChecker "Return checking the program"
    returnCheck prog4

--------------------------------------------------------------------------------
-- Typedefs:
--------------------------------------------------------------------------------

collectTypedefs :: ProgramA -> TCComp ()
collectTypedefs = progCollect _TTypeDef $ \(TypeDef _ typ alias) -> do
    (typ', _) <- inferType typ
    extendTypeDef typeIsReserved alias typ'

expandTypedefs :: ProgramA -> TCComp ProgramA
expandTypedefs = expandInStructs >>. expandTDs

expandInStructs :: TCComp ()
expandInStructs = use structs >>= expandTDs >>= (structs .=)

expandTDs :: Data from => from -> TCComp from
expandTDs = U.transformBiM $ \case
    typ@(TRef _ alias)   -> expandTD typ alias
    x                    -> return x

expandTD :: TypeA -> Ident -> TCComp TypeA
expandTD typ alias = lookupTypeName noSuchTypeName alias >>= \case
    typ'@(TRef _ alias') -> if typ == typ' then return typ
                            else expandTD typ' alias'
    x                    -> return x

nukeTypedefs :: ProgramA -> TCComp ProgramA
nukeTypedefs = pTopDefs %%~ return . filter (isn't _TTypeDef)

--------------------------------------------------------------------------------
-- Structs, Classes:
--------------------------------------------------------------------------------

collectComplex :: ProgramA -> TCComp ()
collectComplex = progCollect _TStructDef $ \(StructDef _ name fields) -> do
    let typ  = appConcrete $ flip TStruct name
    fields' <- forM fields $ sfType %%~ (inferType >$> fst)
    let (nubbed, dups) = nubDupsBy ((==) |. _sfIdent) fields'
    unless (null dups) (structDupFields name nubbed dups)
    extendStruct typeIsReserved typ name fields'

--------------------------------------------------------------------------------
-- Type checking:
--------------------------------------------------------------------------------

checkProg :: ProgramA -> TCComp ProgramA
checkProg = pTopDefs %%~ mapM . toFnDef %%~ \fun ->
    sPushM contexts >> mapM_ extendArg (_fArgs fun) >>
    (fBlock %%~ checkBlock (_fRetTyp fun) $ fun)

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
    Assign   {} -> checkAssign stmt
    SExp     {} -> sExpr %%~ fmap fst . inferExp $ stmt
    Ret      {} -> sExpr %%~ checkExp typ $ stmt
    VRet      _ -> checkVoid typ >> return stmt
    While    {} -> checkC stmt
    Cond     {} -> checkC stmt
    CondElse {} -> checkC >=> checkS sSe $ stmt
    For      {} -> checkFor typ stmt
    where checkC   = sExpr %%~ checkExp bool >=> checkS sSi
          checkS f = f %%~ checkStm typ

checkFor :: TypeA -> StmtA -> TCComp StmtA
checkFor rtyp = sTyp %%~ (inferType >$> fst) >=> \for ->
    let typ = _sTyp for
    in (sExpr %%~ checkExp (grow typ)) for >>= sInScope contexts .
        (extendLocal typ (_sIdent for) >>) . (sSi %%~ checkStm rtyp)

checkAssign :: StmtA -> TCComp StmtA
checkAssign ass = do
    (lval, ltyp) <- inferLVal $ _sLVal ass
    sExpr %%~ checkExp ltyp $ ass { _sLVal = lval }

checkVoid :: TypeA -> TCComp ()
checkVoid frtyp = unless (frtyp == tvoid) (wrongRetTyp tvoid frtyp)

checkDecls :: StmtA -> TCComp StmtA
checkDecls decl = do
    (vtyp', _) <- inferType $ _sDTyp decl
    sDItems %%~ mapM (single vtyp') $ decl { _sDTyp = vtyp' }
    where single vt it = checkDeclItem vt it <* extendLocal vt (_iIdent it)

checkDeclItem :: TypeA -> ItemA -> TCComp ItemA
checkDeclItem _    item@(NoInit _ _) = return item
checkDeclItem vtyp item              = iExpr %%~ checkExp vtyp $ item

checkExp :: TypeA -> ExprA -> TCComp ExprA
checkExp texpected expr = fst <$> unless' (inferExp expr) ((texpected ==) . snd)
                                  (wrongExpTyp expr texpected . snd)

extendArg :: ArgA -> TCComp ()
extendArg a = extendVar' argAlreadyDef $ Var (_aIdent a) (_aTyp a) VSArg 0

extendLocal :: TypeA -> Ident -> TCComp ()
extendLocal typ name = extendVar' varAlreadyDef $ Var name typ VSLocal 0