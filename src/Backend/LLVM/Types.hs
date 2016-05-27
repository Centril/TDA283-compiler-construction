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
Module      : Backend.LLVM.Types
Description : LType and TypeA related in LLVM backend of the Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

LType and TypeA related in LLVM backend of the Javalette compiler.
-}
{-# LANGUAGE LambdaCase, TupleSections #-}

module Backend.LLVM.Types where

import Utils.Monad
import Utils.Sizeables

import Common.AST
import Common.Annotations

import Backend.LLVM.Environment

u = undefined

--------------------------------------------------------------------------------
-- LType related:
--------------------------------------------------------------------------------

compileType :: TypeA -> LComp LType
compileType = \case
    Int      _   -> return intType
    Doub     _   -> return doubType
    Bool     _   -> return boolType
    Void     _   -> return LVoid
    a@Array   {} -> aliasFor a
    s@TStruct {} -> aliasFor s
    r@TRef    {} -> return u -- TODO (classes)
    ConstStr _   -> return strType
    Fun      {}  -> error "compileType Fun not implemented."

boolType, intType, doubType, charType, strType, byteType, bytePType :: LType
boolType  = LInt sizeofBool
intType   = LInt sizeofInt
doubType  = LFloat sizeofFloat
charType  = LInt sizeofChar
strType   = LPtr charType
byteType  = LInt sizeofByte
bytePType = LPtr byteType

sizeofBool, sizeofChar, sizeofByte, sizeofInt, sizeofFloat :: Integer
sizeofBool  = 1
sizeofChar  = 8
sizeofByte  = 8
sizeofInt   = 32
sizeofFloat = 64

compileAnotType :: ASTAnots -> LComp LType
compileAnotType = compileType . getType

--------------------------------------------------------------------------------
-- Aliases:
--------------------------------------------------------------------------------

aliasFor :: TypeA -> LComp LType
aliasFor typ = LPtr . LAlias <$> (getConv typ >>= maybeErr (createAlias typ))

createAlias :: TypeA -> LComp LAliasRef
createAlias typ = case typ of
    Array   {} -> compileType (shrink typ) >>=
                  bindAConv typ . LStruct . (intType:) . return . LArray 0
    TStruct {} -> do
        -- Might have self recursion or even worse: mutual recursion in types.
        -- Thus, create alias first and make it a LPtr to that in aliasFor.
        stAlias <- newAlias <<= insertAConv typ
        _sfType <$$> getStructDef (_tIdent typ) >>= mapM compileType >$> LStruct
            >>= insertAlias stAlias >> return stAlias
    _          -> error "createAlias: should be handled by compileType."