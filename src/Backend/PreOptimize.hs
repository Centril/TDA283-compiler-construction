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

import qualified Data.Generics.Uniplate.Data as U

import Utils.Function

import Common.AST
import Common.Annotations

--------------------------------------------------------------------------------
-- API:
--------------------------------------------------------------------------------

preOptimize :: ProgramA -> ProgramA
preOptimize = stripDeadCode

--------------------------------------------------------------------------------
-- Dead Code Stripping:
--------------------------------------------------------------------------------

-- | 'stripDeadCode': uses annotations to strip dead code.
stripDeadCode :: ProgramA -> ProgramA
stripDeadCode = untilEq $ U.transformBi tBlock . U.transformBi tStmt

tStmt :: StmtA -> StmtA
tStmt = \case
    c@(While    _ e si)    -> stripWhile si e c
    c@(Cond     _ e si)    -> stripIf    si e c
    c@(CondElse _ e si se) -> case extractWE si of
        Just Never         -> wrapBStmt e se
        Just Always        -> wrapBStmt e si
        _                  -> c
    s@(SExp _ e)           -> case extractCELit e of
        Just _             -> always $ Empty emptyAnot
        _                  -> s
    stmt                   -> stmt

stripIf :: StmtA -> ExprA -> StmtA -> StmtA
stripIf s e c = case extractWE s of
    Just Always -> wrapBStmt e s
    Just Never  -> wrapExpr e
    _           -> c

stripWhile :: StmtA -> ExprA -> StmtA -> StmtA
stripWhile s e c = case extractWE s of
    Just Never -> wrapExpr e
    _          -> c

tBlock :: BlockA -> BlockA
tBlock (Block a stmts) = Block a $ stmts >>=
    \case BStmt _ (Block _ ss) -> ss
          Empty _              -> []
          stmt                 -> [stmt]

wrapBStmt :: ExprA -> StmtA -> StmtA
wrapBStmt e s = always $ BStmt emptyAnot $
                always $ Block emptyAnot [wrapExpr e, s]

wrapExpr :: ExprA -> StmtA
wrapExpr e = always $ SExp emptyAnot e