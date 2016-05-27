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
    module Backend.LLVM.AST,

    -- * Types
    LEnv(..), LComp, IOLComp,

    -- * Operations
    initialLEnv,
    getStructDef,

    -- ** Aliases
    bindAConv, insertAConv, getConv,
    bindAlias, insertAlias, newAlias, getAlias, allAliases,

    -- * Temporary SSA
    resetTemp, newTemp,

    -- * Constants
    newConstRef, pushConst, getConsts,

    -- * Instructions
    pushInst, clearInsts, getInsts,

    -- * Labels
    newLabel, newLabels, getLabels, lastLabel
) where

import Prelude hiding (lookup)

import Data.Maybe
import Data.Map (Map, empty, insert, lookup, toList, (!))

import Control.Lens hiding (Context, contexts, pre)

import Utils.Foldable
import Utils.Monad

import Common.AST
import Common.Annotations
import Common.Computation
import Common.StateOps

import Backend.LLVM.AST

--------------------------------------------------------------------------------
-- Operating Environment:
--------------------------------------------------------------------------------

-- | 'StructDefMap': Map from struct type names to its fields.
type StructDefMap = Map Ident [SFieldA]

-- | 'LAConvMap': TypeA in Javalette AST -> alias references in LLVM.
type LAConvMap = Map TypeA LAliasRef

-- | 'LAliasMap': map from a type in Javalette AST to aliases and the
-- corresponding 'LType'.
type LATypeMap = Map LAliasRef LType

-- | 'LEnv': The operating environment of the LLVM computation.
data LEnv = LEnv {
    _constants  :: LConstGlobals,   -- ^ Accumulated list of constants.
    _constCount :: Int,             -- ^ Counter for constants.
    _tempCount  :: Int,             -- ^ Counter for temporary SSA in LLVM.
    _labelCount :: Int,             -- ^ Counter for labels.
    _aliasCount :: Int,             -- ^ Count of aliases.
    _aliasConvs :: LAConvMap,       -- ^ Alias convertion map.
    _aliasTypes :: LATypeMap,       -- ^ Alias type map.
    _structDefs :: StructDefMap,    -- ^ Struct defs, copied from TCEnv.
    _insts      :: LInsts }         -- ^ Accumulated instructions.
    deriving (Eq, Show, Read)

makeLenses ''LEnv

-- | 'initialLEnv': The initial empty LLVM environment.
initialLEnv :: LEnv
initialLEnv = LEnv [] 0 0 0 0 empty empty empty []

--------------------------------------------------------------------------------
-- Environment types:
--------------------------------------------------------------------------------

-- | 'IOLComp': An LLVM computation gifted with the powers of IO, use wisely.
type IOLComp a = IOComp LEnv a

-- | 'LComp': A computation in LLVM code generation using environment 'LEnv'.
type LComp a = Comp LEnv a

--------------------------------------------------------------------------------
-- Struct operations:
--------------------------------------------------------------------------------

getStructDef :: Ident -> LComp [SFieldA]
getStructDef name = uses structDefs (! name)

--------------------------------------------------------------------------------
-- Alias operations:
--------------------------------------------------------------------------------

bindAConv :: TypeA -> LType -> LComp LAliasRef
bindAConv orig conv = bindAlias conv <<= insertAConv orig

insertAConv :: TypeA -> LAliasRef -> LComp ()
insertAConv orig = (aliasConvs %=) . insert orig

getConv :: TypeA -> LComp (Maybe LAliasRef)
getConv = uses aliasConvs . lookup

bindAlias :: LType -> LComp LAliasRef
bindAlias typ = newAlias <<= flip insertAlias typ

insertAlias :: LAliasRef -> LType -> LComp ()
insertAlias alias typ = aliasTypes %= insert alias typ

newAlias :: LComp LAliasRef
newAlias = freshOf "talias" aliasCount

getAlias :: LAliasRef -> LComp (Maybe LType)
getAlias = uses aliasTypes . lookup

allAliases :: LComp LAliases
allAliases = uses aliasTypes toList

--------------------------------------------------------------------------------
-- Temporary SSA operations:
--------------------------------------------------------------------------------

resetTemp :: LComp ()
resetTemp = tempCount .= 0

newTemp :: LComp LIdent
newTemp = freshOf "t" tempCount

--------------------------------------------------------------------------------
-- Const operations:
--------------------------------------------------------------------------------

newConstRef :: String -> LComp LIdent
newConstRef = flip freshOf constCount

pushConst :: LConstGlobal -> LComp ()
pushConst = sAppendL constants

getConsts :: LComp LConstGlobals
getConsts = use constants

--------------------------------------------------------------------------------
-- Instructions operations:
--------------------------------------------------------------------------------

pushInst :: LInst -> LComp ()
pushInst = sAppendL insts

clearInsts :: LComp ()
clearInsts = insts .= []

getInsts :: LComp LInsts
getInsts = use insts

--------------------------------------------------------------------------------
-- Label operations:
--------------------------------------------------------------------------------

newLabel :: String -> LComp LLabelRef
newLabel = flip freshOf labelCount

newLabels :: Functor f => String -> f String -> LComp (f LLabelRef)
newLabels pre ss = addSuffixes ss "." <$> newLabel pre

getLabels :: LComp [LLabelRef]
getLabels = mapMaybe (^? _LLabel) <$> getInsts

lastLabel :: LComp LLabelRef
lastLabel = last <$> getLabels