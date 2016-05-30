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
Module      : Backend.LLVM.AST
Description : The LLVM AST in the LLVM backend of the Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

The LLVM AST in the LLVM backend of the Javalette compiler.
-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.LLVM.AST where

import Control.Lens

--------------------------------------------------------------------------------
-- Data types:
--------------------------------------------------------------------------------

type LLVMCode = String

data LLVMAst = LLVMAst {
      _lGConsts :: LConstGlobals
    , _lAliases :: LAliases
    , _lFDecls  :: LFunDecls
    , _lFDefs   :: LFunDefs }
    deriving (Eq, Ord, Show, Read)

data LConstGlobal = LConstGlobal { _lCGIdent :: LIdent, _lCFVal :: LCValue }
    deriving (Eq, Ord, Show, Read)

type LAlias = (LAliasRef, LType)

data LFunDecl = LFunDecl {
    _lFcRetType :: LType, _lFcIdent :: LIdent, _lFcArgTypes :: LTypes }
    deriving (Eq, Ord, Show, Read)

data LFunDef = LFunDef { _lFdRetType :: LType, _lFdIdent :: LIdent,
    _lFdArgs :: LArgs, _lFdInsts :: LInsts }
    deriving (Eq, Ord, Show, Read)

data LType
    = LVoid
    | LAlias  { _laRef  :: LAliasRef }
    | LInt    { _lIBits :: Integer }
    | LFloat  { _lFBits :: Integer }
    | LPtr    { _lTPtr  :: LType }
    | LFunPtr { _lTFRet :: LType,   _lTFArgs :: [LType] }
    | LArray  { _lADim  :: Integer, _lAType  :: LType }
    | LStruct { _lTypes :: [LType] }
    | LInd
    deriving (Eq, Ord, Show, Read)

data LArg = LArg { _lArgType :: LType, _lArgId :: LIdent }
    deriving (Eq, Ord, Show, Read)

data LICmpOp = LEq | LNe | LUgt | LUge | LUlt | LUle | LSlt | LSgt | LSle | LSge
    deriving (Eq, Ord, Show, Read, Enum)

data LFCmpOp = LFOeq | LFOgt | LFOge | LFOlt | LFOle | LFOne | LFOrd
             | LFUeq | LFUgt | LFUge | LFUlt | LFUle | LFUne | LFUno
    deriving (Eq, Ord, Show, Read, Enum)

data LValRef
    = LVInt   { _lVInt   :: Integer   }
    | LVFloat { _lVFloat :: Double    }
    | LRef    { _lRIdent :: LIdent    }
    | LConst  { _lRIdent :: LIdent    }
    | LVArray { _lvArray :: LTValRefs }
    | LNull
    deriving (Eq, Ord, Show, Read)

data LTValRef = LTValRef { _lTType :: LType, _lTVRef :: LValRef }
    deriving (Eq, Ord, Show, Read)

data LFunRef
    = LFunRef { _lFrDyn :: Bool, _lFrIdent :: LIdent, _lFrArgs :: LTValRefs }
    deriving (Eq, Ord, Show, Read)

data LInst
    = LLabel  { _lLabelRef  :: LLabelRef }
    | LAssign { _lIIdent    :: LIdent,     _lExpr      :: LExpr }
    | LIExpr  { _lExpr      :: LExpr }
    | LVCall  { _lIFunRef   :: LFunRef }
    | LABr    { _lLabelRef  :: LLabelRef }
    | LCBr    { _lITValRef  :: LTValRef,   _lLLabelRef :: LLabelRef,
                _lRLabelRef :: LLabelRef }
    | LVRet
    | LRet    { _lITValRef  :: LTValRef }
    | LStore  { _lILTValRef :: LTValRef,   _lIRTValRef :: LTValRef }
    | LUnreachable
    deriving (Eq, Ord, Show, Read)

data LExpr
    = LLoad     { _lTValRef :: LTValRef }
    | LAlloca   { _lType    :: LType }
    | LCall     { _lType    :: LType,    _lFunRef  :: LFunRef }
    | LBitcast  { _lTValRef :: LTValRef, _lRType   :: LType   }
    | LAdd      { _lTValRef :: LTValRef, _lValRef  :: LValRef }
    | LFAdd     { _lTValRef :: LTValRef, _lValRef  :: LValRef }
    | LSub      { _lTValRef :: LTValRef, _lValRef  :: LValRef }
    | LFSub     { _lTValRef :: LTValRef, _lValRef  :: LValRef }
    | LMul      { _lTValRef :: LTValRef, _lValRef  :: LValRef }
    | LFMul     { _lTValRef :: LTValRef, _lValRef  :: LValRef }
    | LDiv      { _lTValRef :: LTValRef, _lValRef  :: LValRef }
    | LFDiv     { _lTValRef :: LTValRef, _lValRef  :: LValRef }
    | LSRem     { _lTValRef :: LTValRef, _lValRef  :: LValRef }
    | LFRem     { _lTValRef :: LTValRef, _lValRef  :: LValRef }
    | LXor      { _lTValRef :: LTValRef, _lValRef  :: LValRef }
    | LPhi      { _lType    :: LType,    _lPhiRefs :: LPhiRefs }
    | LICmp     { _lICmpOp  :: LICmpOp,  _lTValRef :: LTValRef,
                  _lValRef  :: LValRef }
    | LFCmp     { _lFCmpOp  :: LFCmpOp,  _lTValRef :: LTValRef,
                  _lValRef  :: LValRef }
    | LGElemPtr { _lTValRef :: LTValRef,
                  _lTIndex  :: LTValRef, _lTIndexs :: LTValRefs }
    | LPtrToInt { _lTValRef :: LTValRef, _lRType   :: LType   }
    deriving (Eq, Ord, Show, Read)

data LPhiRef = LPhiRef LValRef LLabelRef
    deriving (Eq, Ord, Show, Read)

type LLabelRef = LIdent
type LAliasRef = LIdent
type LIdent  = String

type LConstGlobals = [LConstGlobal]
type LAliases      = [LAlias]
type LFunDecls     = [LFunDecl]
type LFunDefs      = [LFunDef]
type LTValRefs     = [LTValRef]
type LPhiRefs      = [LPhiRef]
type LInsts        = [LInst]
type LTypes        = [LType]
type LArgs         = [LArg]
type LCValue       = LTValRef

--------------------------------------------------------------------------------
-- Lenses and Prisms:
--------------------------------------------------------------------------------

concat <$> mapM (\n -> (++) <$> makeLenses n <*> makePrisms n)
    [''LLVMAst, ''LConstGlobal, ''LFunDecl, ''LFunDef, ''LType, ''LArg
    ,''LICmpOp, ''LFCmpOp, ''LValRef, ''LTValRef, ''LFunRef,''LInst, ''LExpr
    ,''LPhiRef]