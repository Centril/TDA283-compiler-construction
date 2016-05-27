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
Module      : Backend.LLVM.CodeDSL
Description : Basic LLVM DSL in LLVM backend of the Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Basic LLVM DSL in LLVM backend of the Javalette compiler.
-}
{-# LANGUAGE LambdaCase, TupleSections #-}

module Backend.LLVM.CodeDSL where

import Utils.Pointless
import Utils.Monad

import Backend.LLVM.Environment
import Backend.LLVM.Types

u = undefined

--------------------------------------------------------------------------------
-- Branching, Labels:
--------------------------------------------------------------------------------

compileLabel :: LLabelRef -> LComp ()
compileLabel l = pushInst $ LLabel l

compileLabelJmp :: LLabelRef -> LComp ()
compileLabelJmp l = aBr l >> compileLabel l

xInLabel :: LLabelRef -> LLabelRef -> LComp b -> LComp b
xInLabel _lab _cont x = compileLabel _lab >> x <* aBr _cont

condBr :: LLabelRef -> LLabelRef -> LTValRef -> LComp ()
condBr _then _else r = pushInst $ LCBr r _then _else

aBr :: LLabelRef -> LComp ()
aBr = pushInst . LABr

--------------------------------------------------------------------------------
-- Pointers:
--------------------------------------------------------------------------------

deref :: LTValRefs -> LTValRef -> LExpr
deref = flip $ flip LGElemPtr izero

ptrTo :: LType -> LValRef -> LTValRef
ptrTo = LTValRef . LPtr

ptrRef :: LType -> LIdent -> LTValRef
ptrRef typ = ptrTo typ . LRef

strPointer :: LType -> LIdent -> LExpr
strPointer t = deref [izero] . LTValRef t . LConst

--------------------------------------------------------------------------------
-- Constant values + Constructors:
--------------------------------------------------------------------------------

izero, ione, dzero, ltrue, lfalse :: LTValRef
izero  = intTVR 0
ione   = intTVR 1
dzero  = doubTVR 0
ltrue  = boolTVR True
lfalse = boolTVR False

intTVR :: Integer -> LTValRef
intTVR = LTValRef intType . LVInt

boolTVR :: Bool -> LTValRef
boolTVR = LTValRef boolType . LVInt . toInteger . fromEnum

doubTVR :: Double -> LTValRef
doubTVR = LTValRef doubType . LVFloat

--------------------------------------------------------------------------------
-- Basic operations:
--------------------------------------------------------------------------------

iadd, imul :: LTValRef -> LTValRef -> LComp LTValRef
iadd a (LTValRef _ b) = assignInt $ LAdd a b
imul a (LTValRef _ b) = assignInt $ LMul a b

ptrToInt :: LTValRef -> LComp LTValRef
ptrToInt = assignInt . flip LPtrToInt intType

bitcast :: LType -> LTValRef -> LComp LTValRef
bitcast to ptr = assignTemp to (LBitcast ptr to)

alloci :: LComp LTValRef
alloci = assignIntP $ LAlloca intType

loadi :: LTValRef -> LComp LTValRef
loadi = assignInt . LLoad

load :: LType -> LTValRef -> LComp LTValRef
load typ = assignTemp typ . LLoad

assignBool, assignInt, assignIntP :: LExpr -> LComp LTValRef
assignBool = assignTemp boolType
assignInt  = assignTemp intType
assignIntP = assignPtr  intType

assignPtr :: LType -> LExpr -> LComp LTValRef
assignPtr = assignTemp . LPtr

assignCall :: LType -> LFunRef -> LComp LTValRef
assignCall rtyp = assignTemp rtyp . LCall rtyp

vcall :: LFunRef -> LComp LTValRef
vcall fr = pushInst (LVCall fr) >> return (LTValRef LVoid LNull)

assignTemp :: LType -> LExpr -> LComp LTValRef
assignTemp rtyp expr = LTValRef rtyp . LRef <$> newTemp <<= flip assignE expr

alloc :: LIdent -> LType -> LComp ()
alloc n = assignE n . LAlloca

assignE :: LIdent -> LExpr -> LComp ()
assignE n = pushInst . LAssign n

store :: LTValRef -> LTValRef -> LComp ()
store = pushInst .| LStore