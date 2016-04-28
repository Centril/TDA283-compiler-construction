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
Module      : Backend.PreOpt
Description : Pre optimizations in backend of Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Pre optimizations in backend of Javalette compiler.
-}
{-# LANGUAGE LambdaCase #-}
module Backend.PreOpt (
    -- * Operations
    preOpt
) where

import Data.Data
import qualified Data.Generics.Uniplate.Data as U

import Control.Monad

import Control.Lens hiding (from, to, Empty)

import Utils.Foldable

import Frontend.Annotations

import Javalette.Abs

--------------------------------------------------------------------------------
-- API:
--------------------------------------------------------------------------------

preOpt :: ProgramA -> ProgramA
preOpt = deadNuke

--------------------------------------------------------------------------------
-- Dead Nuking:
--------------------------------------------------------------------------------

-- | 'deadNuke': uses annotations to nuke away dead code.
deadNuke :: ProgramA -> ProgramA
deadNuke = untilSame $ tBlock *.* tStmt

f *.* g = U.transformBi f . U.transformBi g

tStmt :: StmtA -> StmtA
tStmt = \case
    c@(While _ e s) -> nukeCond s e c
    c@(Cond _ e s) -> nukeCond s e c
    c@(CondElse _ e si se) -> case getWE si of
        Just Never  -> wrapBStmt e se
        Just Always -> wrapBStmt e si
        _          -> c
    s@(SExp _ e)   -> case getCLit e of
        Just _     -> always $ Empty emptyAnot
        _          -> s
    stmt           -> stmt

nukeCond :: Data (f ASTAnots) => f ASTAnots -> ExprA -> StmtA -> StmtA
nukeCond s e c = case getWE s of
    Just Never -> wrapExpr e
    _          -> c

tBlock :: BlockA -> BlockA
tBlock (Block a stmts) = Block a $ stmts >>=
    \case BStmt _ (Block _ ss) -> ss
          Empty _              -> []
          stmt                 -> [stmt]

untilSame :: Eq a => (a -> a) -> a -> a
untilSame f x = if c == x then x else untilSame f c
    where c = f x

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