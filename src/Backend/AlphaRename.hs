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
import qualified Data.Map as M

import Control.Arrow
import Control.Monad
import Control.Monad.State

import Control.Lens hiding (from, to)

import Utils.Monad
import Utils.Foldable
import Utils.Pointless

import Common.StateOps
import Common.AST
import Common.Annotations

-- TODO: reorganize?
import Frontend.Environment
import qualified Utils.GraphFlip as GF

u = undefined

--------------------------------------------------------------------------------
-- Operating Environment:
--------------------------------------------------------------------------------

-- | 'AREnv': The operating environment of an alpha renaming computation.
data AREnv = AREnv {
      _magic     :: M.Map Ident Ident
    , _substs    :: [M.Map Ident Ident]
    , _nameCount :: Int }

makeLenses ''AREnv

-- | 'ARComp': The type of computations for alpha renaming.
type ARComp a = State AREnv a

--------------------------------------------------------------------------------
-- API:
--------------------------------------------------------------------------------

-- | 'alphaRename': alpha renames a 'ProgramA' plus the classes.
alphaRename :: ProgramA -> TCComp ProgramA
alphaRename p = do
    classGraph %= second (`GF.nmap` arX arClass)
    return $ arX arTop p

--------------------------------------------------------------------------------
-- Special cases:
--------------------------------------------------------------------------------

arX :: (a -> ARComp c) -> a -> c
arX f = flip evalState (AREnv M.empty [] 0) . f

prereservedIdents :: [Ident]
prereservedIdents = Ident <$> ["self", "this"]

-- hlint is wrong, brackets are NOT redundant.
arClass :: ClassInfo -> ARComp ClassInfo
arClass = mapM_ bindMagic prereservedIdents                 >>.
          sInScope substs     . (
              mapM_ idSub     . (_ciFields >$> _sfIdent)    <=>
              sInScope substs . (ciMethods %%~ mapM arFun)) .<*
          nukeMagic

arTop :: ProgramA -> ARComp ProgramA
arTop = pTopDefs %%~ mapM . toFnDef %%~ arFun

--------------------------------------------------------------------------------
-- Functions:
--------------------------------------------------------------------------------

arFun :: FnDefA -> ARComp FnDefA
arFun f = sPushM substs >> arF f <* (nameCount .= 0)

arF :: FnDefA -> ARComp FnDefA
arF     = fArgs %%~ mapM arArg >=> fBlock %%~ arBlock

arArg :: ArgA -> ARComp ArgA
arArg   = aIdent %%~ newSub

arBlock :: BlockA -> ARComp BlockA
arBlock b = (bStmts %%~ mapM arStmt) b <* sPopM substs

arStmt :: StmtA -> ARComp StmtA
arStmt stmt = case stmt of
    BStmt    {} -> sPushM substs >> (sBlock %%~ arBlock $ stmt)
    Decl     {} -> sDItems %%~ mapM arItem $ stmt
    Assign   {} -> sLVal %%~ arLVal >=> arE $ stmt
    SExp     {} -> arE stmt
    Ret      {} -> arE stmt
    While    {} -> arC stmt
    Cond     {} -> arC stmt
    CondElse {} -> arC >=> sSe %%~ arStmt $ stmt
    For      {} -> arE >=> sInScope substs . (sIdent %%~ newSub >=> arSi) $ stmt
    _           -> return stmt
    where arC  = arE >=> arSi
          arSi = sSi %%~ arStmt
          arE  = sExpr %%~ arExpr

arItem :: ItemA -> ARComp ItemA
arItem i = (case i of Init {} -> iExpr %%~ arExpr $ i; _ -> return i) >>=
           (iIdent %%~ newSub)

arExpr :: ExprA -> ARComp ExprA
arExpr expr = case expr of
    ENew      {} -> eDimEs %%~ mapM (deExpr %%~ arExpr) $ expr
    EVar      {} -> arL
    ELitInt   {} -> r
    ELitDoub  {} -> r
    EString   {} -> r
    ELitTrue  {} -> r
    ELitFalse {} -> r
    ECastNull {} -> r
    EApp      {} -> arA expr
    EMApp     {} -> arA >=> eExpr %%~ arExpr $ expr
    Incr      {} -> arL
    Decr      {} -> arL
    PreIncr   {} -> arL
    PreDecr   {} -> arL
    Neg       {} -> arE
    Not       {} -> arE
    EMul      {} -> arB
    EAdd      {} -> arB
    ERel      {} -> arB
    EAnd      {} -> arB
    EOr       {} -> arB
    where arE = eExpr  %%~ arExpr $ expr
          arB = eLExpr %%~ arExpr >=> eRExpr %%~ arExpr $ expr
          arL = eLVal  %%~ arLVal $ expr
          arA = eAppExprs %%~ mapM arExpr
          r   = return expr

arLVal :: LValueA -> ARComp LValueA
arLVal lv = case lv of
    LValueV {} -> lvIdent %%~ arRef >=>
                  lvDimEs %%~ mapM (deExpr %%~ arExpr) $ lv
    LValueS {} -> -- TODO alpha rename ONLY expressions on rhs.
                  lvLLVal %%~ arLVal $ lv

--------------------------------------------------------------------------------
-- Building blocks:
--------------------------------------------------------------------------------

arRef :: Ident -> ARComp Ident
arRef i = uses magic (M.lookup i) >>=
          maybeErr (uses substs $ fromJust .| ctxFirst $ i)

bindSub :: Ident -> Ident -> ARComp ()
bindSub x = (substs %=) . modifyf . M.insert x

newSub :: Ident -> ARComp Ident
newSub from = (Ident <$> freshOf "v" nameCount) <<= bindSub from

idSub :: Ident -> ARComp ()
idSub from = bindSub from from

bindMagic :: Ident -> ARComp Ident
bindMagic from = return from <<= (magic %=) . M.insert from

nukeMagic :: ARComp ()
nukeMagic = magic .= M.empty