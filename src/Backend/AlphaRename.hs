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

import Utils.List
import Utils.Pointless

import Common.StateOps
import Common.Uniplate()
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
alphaRename = flip evalState (AREnv [] 0) . arProg

--------------------------------------------------------------------------------
-- Alpha Renaming:
--------------------------------------------------------------------------------

-- | 'ARComp': The type of computations for alpha renaming.
type ARComp a = State AREnv a

arRef :: Ident -> ARComp Ident
arRef i = uses substs $ fromJust .| ctxFirst $ i

newSubst :: Ident -> ARComp Ident
newSubst from = do to <- Ident <$> freshOf "v" nameCount
                   substs %= modifyf (insert from to)
                   return to

arProg :: ProgramA -> ARComp ProgramA
arProg (Program a funs) = Program a <$> mapM arFun funs

arFun :: TopDefA -> ARComp TopDefA
arFun (FnDef a r i p b) = sPushM substs >>
                          liftM2 (FnDef a r i) (mapM arArg p) (arBlock b) <*
                          (nameCount .= 0)

arArg :: ArgA -> ARComp ArgA
arArg (Arg a t i) = Arg a t <$> newSubst i

arBlock :: BlockA -> ARComp BlockA
arBlock (Block a block) = Block a <$> mapM arStmt block <* sPopM substs

arStmt :: StmtA -> ARComp StmtA
arStmt stmt = case stmt of
    BStmt    a b       -> sPushM substs >> BStmt a <$> arBlock b
    Decl     a t is    -> Decl a t <$> mapM arItem is
    Ass      a i e     -> liftM2 (Ass a) (arRef i) (arExpr e)
    Incr     a i       -> Incr a <$> arRef i
    Decr     a i       -> Decr a <$> arRef i
    SExp     a e       -> SExp a <$> arExpr e
    Ret      a e       -> Ret  a <$> arExpr e
    While    a e s     -> arC (While    a) e s
    Cond     a e s     -> arC (Cond     a) e s
    CondElse a e si se -> arC (CondElse a) e si <*> arStmt se
    _                  -> return stmt
    where arC ctor e s = ctor <$> arExpr e <*> arStmt s

arItem :: ItemA -> ARComp ItemA
arItem (Init   a i e) = liftM2 (flip (Init a)) (arExpr e) (newSubst i)
arItem (NoInit a i  ) = NoInit a <$> newSubst i

arExpr :: ExprA -> ARComp ExprA
arExpr = U.transformM $ \case EVar a i -> EVar a <$> arRef i
                              x        -> return x