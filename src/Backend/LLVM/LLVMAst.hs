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
Module      : Backend.LLVM.LLVMAst
Description : The LLVM AST in the LLVM backend of the Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

The LLVM AST in the LLVM backend of the Javalette compiler.
-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.LLVM.LLVMAst where

import Control.Lens

data LLVMAst = LLVMAst {
    _lGConsts :: LConstGlobals, _lFDecls :: LFunDecls, _lFDefs :: LFunDefs }
    deriving (Eq, Ord, Show, Read)

data LConstGlobal = LConstGlobal {
    _lCGIdent :: LIdent, _lCGType :: LType, _lCFVal :: LValue }
    deriving (Eq, Ord, Show, Read)

data LFunDecl = LFunDecl {
    _lFcRetType :: LType, _lFcIdent :: LIdent, _lFcArgTypes :: LTypes }
    deriving (Eq, Ord, Show, Read)

data LFunDef = LFunDef {
    _lFdRetType :: LType, _lFdIdent :: LIdent, _lFdArgs :: LArgs,
    _lFdInsts :: LInsts }
    deriving (Eq, Ord, Show, Read)

data LType
    = LVoid
    | LInt { _lIBits :: Int }
    | LFloat { _lFBits :: Int }
    | LPtr { _lTPtr :: LType }
    | LArray { _lADim :: Int, _lAType :: LType }
    deriving (Eq, Ord, Show, Read)

data LArg = LArg { _lArgType :: LType, _lArgId :: LIdent }
    deriving (Eq, Ord, Show, Read)

data LOp = LEq | LNe | LUlt | LSgt | LUle | LSge
    deriving (Eq, Ord, Show, Read, Enum)

data LValRef
    = LVInt { _lVInt :: Integer }
    | LVFloat { _lVFloat :: Double }
    | LRef { _lRIdent :: LIdent }
    | LNull
    deriving (Eq, Ord, Show, Read)

data LTValRef = LTValRef { _lTType :: LType, _lTVRef :: LValRef }
    deriving (Eq, Ord, Show, Read)

data LFunRef = LFunRef {
    _lFrIdent :: LIdent, _lFrArgs :: LTValRefs }
    deriving (Eq, Ord, Show, Read)

-- TODO: Name the expressions
data LInst
    = LLabel { _lLaIdent :: LLabelRef }
    | LAssign { _lInIdent :: LIdent, _lInExpr :: LExpr }
    | LIExpr LExpr
    | LVCall LFunRef
    | LABr LLabelRef
    | LCBr LTValRef LLabelRef LLabelRef
    | LVRet
    | LRet LTValRef
    | LStore LTValRef LTValRef
    | LUnreachable
    deriving (Eq, Ord, Show, Read)

data LExpr
    = LLoad LTValRef
    | LAlloca LType
    | LCall LType LFunRef
    | LBitcast LType LValRef LType
    | LAdd LTValRef LValRef
    | LFAdd LTValRef LValRef
    | LSub LTValRef LValRef
    | LFSub LTValRef LValRef
    | LMul LTValRef LValRef
    | LFMul LTValRef LValRef
    | LDiv LTValRef LValRef
    | LFDiv LTValRef LValRef
    | LXor LTValRef LValRef
    | LPhi LType LPhiRefs
    | LICmp LOp LTValRef LValRef
    | LFCmp LOp LTValRef LValRef
    | LGElemPtr LType LIdent LTIndex [LTIndex]
    | LPtrToInt LType LValRef LType
    deriving (Eq, Ord, Show, Read)

data LPhiRef = LPhiRef LValRef LLabelRef
    deriving (Eq, Ord, Show, Read)

type LLabelRef = LIdent

type LValue  = String
type LIdent  = String
type LIndex  = Int
type LTIndex = (LType, LIndex)

type LConstGlobals = [LConstGlobal]
type LFunDecls     = [LFunDecl]
type LFunDefs      = [LFunDef]
type LTValRefs     = [LTValRef]
type LPhiRefs      = [LPhiRef]
type LInsts        = [LInst]
type LTypes        = [LType]
type LArgs         = [LArg]

makePrisms ''LInst