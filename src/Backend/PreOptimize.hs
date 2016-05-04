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
Module      : Backend.PreOptimize
Description : Pre optimizations in backend of Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Pre optimizations in backend of Javalette compiler.
-}
{-# LANGUAGE LambdaCase #-}
module Backend.PreOptimize (
    -- * Operations
    preOptimize
) where

import Data.Data
import qualified Data.Generics.Uniplate.Data as U

import Control.Monad

import Control.Lens hiding (from, to, Empty)

import Utils.Foldable
import Utils.Function

import Common.AST

import Frontend.Annotations

--------------------------------------------------------------------------------
-- API:
--------------------------------------------------------------------------------

preOptimize :: ProgramA -> ProgramA
preOptimize = stripDeadCode

--------------------------------------------------------------------------------
-- Dead Nuking:
--------------------------------------------------------------------------------

-- | 'stripDeadCode': uses annotations to strip dead code.
stripDeadCode :: ProgramA -> ProgramA
stripDeadCode = untilEq $ U.transformBi tBlock . U.transformBi tStmt

tStmt :: StmtA -> StmtA
tStmt = \case
    c@(While _ e s) -> stripCond s e c
    c@(Cond _ e s)         -> stripCond s e c
    c@(CondElse _ e si se) -> case getWE si of
        Just Never         -> wrapBStmt e se
        Just Always        -> wrapBStmt e si
        _                  -> c
    s@(SExp _ e)           -> case getCLit e of
        Just _             -> always $ Empty emptyAnot
        _                  -> s
    stmt                   -> stmt

stripCond :: Data (f ASTAnots) => f ASTAnots -> ExprA -> StmtA -> StmtA
stripCond s e c = case getWE s of
    Just Never -> wrapExpr e
    _          -> c

tBlock :: BlockA -> BlockA
tBlock (Block a stmts) = Block a $ stmts >>=
    \case BStmt _ (Block _ ss) -> ss
          Empty _              -> []
          stmt                 -> [stmt]

always :: Annotated f => f ASTAnots -> f ASTAnots
always = addWE Always

wrapBStmt :: ExprA -> StmtA -> StmtA
wrapBStmt e s = always $ BStmt emptyAnot $
                always $ Block emptyAnot [wrapExpr e, s]

wrapExpr :: ExprA -> StmtA
wrapExpr e = always $ SExp emptyAnot e

getWE :: AnotExtract f => f ASTAnots -> Maybe WillExecute
getWE = mfind (^? _AWillExec) . extractAnot

getCLit :: AnotExtract f => f ASTAnots -> ML
getCLit = join . mfind (^? _ACExprLit) . extractAnot