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
module Backend.LLVM.LLVMAst where

data LLVMAst = LLVMAST {
    _lGConsts :: LConstGlobals, _lFDecls :: LFunDecls, _lFDefs :: LFunDefs }
    deriving (Eq, Ord, Show, Read)

data LConstGlobal = LConstGlobal {
    _lCGType :: LType, _lCGIdent :: LIdent, _lCFVal :: LValue }
    deriving (Eq, Ord, Show, Read)

data LFunDecl = LFunDecl {
    _lFcRetType :: LType, _lFcIdent :: LIdent, _lFcArgType :: LTypes }
    deriving (Eq, Ord, Show, Read)

data LFunDef = LFunDef {
    _lFdRetType :: LType, _lFdIdent :: LIdent, _lFdArgs :: LArgs,
    _lFdLabels :: LLabels }
    deriving (Eq, Ord, Show, Read)

data LType = LVoid                                      |
             LInt { _lIBits :: Int }                    |
             LFloat { _lFBits :: Int }                  |
             LPtr { _lTPtr :: LType }                   |
             LArray { _lADim :: Int, _lAType :: LType }
    deriving (Eq, Ord, Show, Read)

data LArg = LArg { _lArgId :: LIdent, _lArgType :: LType }
    deriving (Eq, Ord, Show, Read)

data LLabel = LLabel { _lLaIdent :: LIdent, _lLaInsts :: LInsts }
    deriving (Eq, Ord, Show, Read)

data LOp = LEq | LNe | LUlt | LSgt | LUle | LSge
    deriving (Eq, Ord, Show, Read, Enum)

data LValRef = LVInt { _lVInt :: Int }       |
               LVFloat { _lVFloat :: Float } |
               LRef { _lRIdent :: LIdent }
    deriving (Eq, Ord, Show, Read)

data LTValRef = LTValRef { _lTType :: LType, _lTVRef :: LValRef }
    deriving (Eq, Ord, Show, Read)

data LFunRef = LFunSig {
    _lFrIdent :: LIdent, _lFrArgRef :: LTValRef }
    deriving (Eq, Ord, Show, Read)

-- TODO: Name the expressions
data LInst = LAssign { _lInIdent :: LIdent, _lInExpr :: LExpr } |
             LIExpr LExpr                                       |
             LVCall LFunRef                                     |
             LABr LLabelRef                                     |
             LCBr LTValRef LLabelRef LLabelRef                  |
             LRet LTValRef                                      |
             LStore LTValRef LTValRef                           |
             LUnreachable
             deriving (Eq, Ord, Show, Read)

data LExpr = LLoad LTValRef                           |
             LAlloca LType                            |
             LCall LFunRef                            |
             LBitcast LType LValRef LType             |
             LAdd LTValRef LValRef                    |
             LFAdd LTValRef LValRef                   |
             LSub LTValRef LValRef                    |
             LFSub LTValRef LValRef                   |
             LMul LTValRef LValRef                    |
             LFMul LTValRef LValRef                   |
             LDiv LTValRef LValRef                    |
             LFDiv LTValRef LValRef                   |
             LICmp LOp LTValRef LValRef               |
             LFCmp LOp LTValRef LValRef               |
             LGElemPtr LType LIdent LTIndex [LTIndex] |
             LPtrToInt LType LValRef LType
             deriving (Eq, Ord, Show, Read)

type LValue = String
type LLabelRef = LIdent
type LIdent = String
type LIndex = Int
type LTIndex = (LType, LIndex)

type LConstGlobals = [LConstGlobal]
type LFunDecls     = [LFunDecl]
type LFunDefs      = [LFunDef]

type LLabels = [LLabel]
type LInsts  = [LInst]
type LTypes  = [LType]
type LArgs   = [LArg]