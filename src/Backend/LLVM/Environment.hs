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
    -- * Modules
    module Common.Computation,
    module Backend.LLVM.LLVMAst,

    -- * Types
    LEnv(..), LComp, LResult, IOLComp, IOLResult,

    -- * Operations
    initialLEnv,
    resetTemp, newTemp,
    newLabel, newLabels, getLabels, lastLabel,
    newConstRef, pushConst,
    pushInst, clearInsts, getInsts
) where

import Data.Maybe

import Control.Lens hiding (Context, contexts, pre)

import Utils.Foldable

import Common.Computation
import Common.StateOps

import Backend.LLVM.LLVMAst

--------------------------------------------------------------------------------
-- Operating Environment:
--------------------------------------------------------------------------------

-- TODO: Move FnSigMap to Common
-- | 'LEnv': The operating environment of the LLVM computation.
data LEnv = LEnv {
    _constants  :: LConstGlobals,   -- ^ Accumulated list of constants.
    _constCount :: Int,             -- ^ Counter for constants.
    _tempCount  :: Int,             -- ^ Counter for temporary SSA in LLVM.
    _labelCount :: Int,             -- ^ Counter for labels.
    _insts      :: LInsts }         -- ^ Accumulated instructions.
    deriving (Eq, Show, Read)

makeLenses ''LEnv

-- | 'initialLEnv': The initial empty LLVM environment.
initialLEnv :: LEnv
initialLEnv = LEnv [] 0 0 0 []

--------------------------------------------------------------------------------
-- Environment operations:
--------------------------------------------------------------------------------

-- | 'IOLComp': An LLVM computation gifted with the powers of IO, use wisely.
type IOLComp a = IOComp LEnv a

-- | 'IOLResult' result of an 'IOLComp' computation.
type IOLResult a = IOCompResult LEnv a

-- | 'LComp': A computation in LLVM code generation using environment 'LEnv'.
type LComp a = Comp LEnv a

-- | 'LResult': result of an 'LComp' computation.
type LResult a = CompResult LEnv a

resetTemp :: LComp ()
resetTemp = tempCount .= 0

newTemp :: LComp LIdent
newTemp = freshOf "t" tempCount

newLabel :: String -> LComp LLabelRef
newLabel = flip freshOf labelCount

newLabels :: Functor f => String -> f String -> LComp (f LLabelRef)
newLabels pre ss = addSuffixes ss "." <$> newLabel pre

newConstRef :: String -> LComp LIdent
newConstRef = flip freshOf constCount

pushConst :: LConstGlobal -> LComp ()
pushConst = sAppendL constants

pushInst :: LInst -> LComp ()
pushInst = sAppendL insts

clearInsts :: LComp ()
clearInsts = insts .= []

getInsts :: LComp LInsts
getInsts = use insts

getLabels :: LComp [LLabelRef]
getLabels = mapMaybe (^? _LLabel) <$> getInsts

lastLabel :: LComp LLabelRef
lastLabel = head <$> getLabels