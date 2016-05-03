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
Module      : Backend.LLVM.Environment
Description : Operating environment of LLVM Backend in Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Operating environment of LLVM Backend in Javalette compiler.
-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.LLVM.Environment (
    -- * Types
    LEnv(..),

    -- * Operations
    initialLEnv
) where

import Data.Map ((!))

import Control.Lens hiding (Context, contexts)

import Common.AST
import Common.Computation
import Common.Abcd

import Frontend.Environment

import Backend.LLVM.LLVMAst

--------------------------------------------------------------------------------
-- Operating Environment:
--------------------------------------------------------------------------------

-- TODO: Move FnSigMap to Common
-- | 'LEnv': The operating environment of the LLVM computation.
data LEnv = LEnv {
    _constants  :: LConstGlobals, -- ^ Accumulated list of constants.
    _lfunctions :: FnSigMap,      -- ^ Map of ident -> function signatures.
    _tempCount  :: Int,           -- ^ Counter for temporary SSA in LLVM.
    _labelCount :: Int,           -- ^ Counter for labels.
    _labels     :: LLabels }      -- ^ Accumulated labels.
    deriving (Eq, Show, Read)

makeLenses ''LEnv

-- | 'initialLEnv': The initial empty LLVM environment.
initialLEnv :: FnSigMap -> LEnv
initialLEnv fns = LEnv [] fns 0 0 []

--------------------------------------------------------------------------------
-- Environment operations:
--------------------------------------------------------------------------------

type LComp a = Comp LEnv a

lookupLFun :: Ident -> LComp FunSig
lookupLFun = uses lfunctions . flip (!)

newTemp :: LComp LIdent
newTemp = freshOf "t" tempCount

newLabel :: String -> LComp LLabelRef
newLabel = flip freshOf labelCount

pushConst :: LConstGlobal -> LComp ()
pushConst = sAppendL constants

pushLabel :: LLabel -> LComp ()
pushLabel = sAppendL labels