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
Module      : Backend.AlphaRename
Description : Alpha renaming in backend of Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Alpha renaming in backend of Javalette compiler.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Backend.AlphaRename (
    -- * Operations
    alphaRename
) where

import Data.Maybe
import Data.Map (Map, insert)

import qualified Data.Generics.Uniplate.Data as U

import Control.Monad
import Control.Monad.State

import Control.Lens hiding (from, to)

import Utils.Monad
import Utils.Foldable
import Utils.Pointless

import Common.StateOps
import Common.AST

import Frontend.Annotations

--------------------------------------------------------------------------------
-- Operating Environment:
--------------------------------------------------------------------------------

-- | 'AREnv': The operating environment of an alpha renaming computation.
data AREnv = AREnv { _substs :: [Map Ident Ident], _nameCount :: Int }
makeLenses ''AREnv

--------------------------------------------------------------------------------
-- API:
--------------------------------------------------------------------------------

-- | 'alphaRename': alpha renames a 'ProgramA'.
alphaRename :: ProgramA -> ProgramA
alphaRename = flip evalState (AREnv [] 0) . (pTopDefs %%~ mapM arFun)
    where arFun f = sPushM substs >> arF f <* (nameCount .= 0)
          arF     = fArgs %%~ mapM arArg >=> fBlock %%~ arBlock
          arArg   = aIdent %%~ newSubst

--------------------------------------------------------------------------------
-- Alpha Renaming:
--------------------------------------------------------------------------------

-- | 'ARComp': The type of computations for alpha renaming.
type ARComp a = State AREnv a

arRef :: Ident -> ARComp Ident
arRef i = uses substs $ fromJust .| ctxFirst $ i

newSubst :: Ident -> ARComp Ident
newSubst from = (Ident <$> freshOf "v" nameCount) <<=
                (substs %=) . modifyf . insert from

arBlock :: BlockA -> ARComp BlockA
arBlock b = (bStmts %%~ mapM arStmt) b <* sPopM substs

arStmt :: StmtA -> ARComp StmtA
arStmt stmt = case stmt of
    BStmt    {} -> sPushM substs >> (sBlock %%~ arBlock $ stmt)
    Decl     {} -> sDItems %%~ mapM arItem $ stmt
    Ass      {} -> arI >=> arE $ stmt
    Incr     {} -> arI stmt
    Decr     {} -> arI stmt
    SExp     {} -> arE stmt
    Ret      {} -> arE stmt
    While    {} -> arC stmt
    Cond     {} -> arC stmt
    CondElse {} -> arC >=> sSe %%~ arStmt $ stmt
    _           -> return stmt
    where arC = arE >=> sSi %%~ arStmt
          arE = sExpr %%~ arExpr
          arI = sIdent %%~ arRef

arItem :: ItemA -> ARComp ItemA
arItem i = (case i of Init {} -> iExpr %%~ arExpr $ i; _ -> return i) >>=
           (iIdent %%~ newSubst)

arExpr :: ExprA -> ARComp ExprA
arExpr = U.transformM $ \case EVar a i -> EVar a <$> arRef i
                              x        -> return x