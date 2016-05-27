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
Module      : Backend.LLVM.CodeComplex
Description : Codegen: Complex types in LLVM backend of the Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Codegen: Complex types in LLVM backend of the Javalette compiler.
-}
{-# LANGUAGE LambdaCase, TupleSections #-}

module Backend.LLVM.CodeComplex where

import Control.Monad

import Utils.Monad

import Backend.LLVM.Environment
import Backend.LLVM.Types
import Backend.LLVM.CodeDSL

u = undefined

--------------------------------------------------------------------------------
-- Structs, Arrays:
--------------------------------------------------------------------------------

compileSizeof :: LType -> LComp LTValRef
compileSizeof t = assignTemp t (LGElemPtr (LTValRef t LNull) ione []) >>=
                  ptrToInt

compileCalloc :: LTValRef -> LTValRef -> LComp LTValRef
compileCalloc n sizeof = assignCall bytePType $ LFunRef "calloc" [n, sizeof]

lenSize :: LTValRef
lenSize = intTVR $ sizeofInt `div` 8

loadLength :: LTValRef -> LComp LTValRef
loadLength = lengthRef >=> loadi

setLength :: LTValRef -> LTValRef -> LComp ()
setLength len = lengthRef >=> store len

lengthRef :: LTValRef -> LComp LTValRef
lengthRef = assignIntP . deref [izero]

basicFor :: LLabelRef -> LType -> LTValRef -> LTValRef
        -> (LTValRef -> LComp ()) -> LComp ()
basicFor prefix lbt arr len handler = do
    [_check, _then, _cont] <- newLabels prefix ["check", "then", "cont"]
    -- set: i = 0
    i     <- alloci <<= store izero
    compileLabelJmp _check
    -- check: i < arr.length
    iload <- loadi i
    assignBool (LICmp LSlt iload $ _lTVRef len) >>= condBr _then _cont
    -- load: arr[i] + body + store: i++
    xInLabel _then _check $ assignPtr lbt (deref [ione, iload] arr) >>= handler
                            >> iadd iload ione >>= flip store i
    compileLabel _cont