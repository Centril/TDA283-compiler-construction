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
Module      : Frontend.TypeFunSig
Description : Collecting function signatures & checking main.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Collecting function signatures & checking main.
-}
module Frontend.TypeFunSig (
    -- * Operations
    mainCorrect, allFunctions
) where

import Control.Monad

import Javalette.Abs

import Frontend.Annotations
import Frontend.Types
import Frontend.Query
import Frontend.Error
import Frontend.Common

--------------------------------------------------------------------------------
-- Checking for int main(void):
--------------------------------------------------------------------------------

mainId :: Ident
mainId = Ident "main"

mainCorrect :: Eval ()
mainCorrect = lookupFunE mainId >>=
    flip unless wrongMainSig . (== FunSig [] (Int emptyAnot))

--------------------------------------------------------------------------------
-- Collecting function signatures:
--------------------------------------------------------------------------------

allFunctions :: ProgramA -> Eval ()
allFunctions = collectFuns . (predefFuns ++) . extractFunIds

collectFuns :: [FunId] -> Eval ()
collectFuns = mapM_ $ extendFun' funAlreadyDef

predefFuns :: [FunId]
predefFuns = map toFunId
    [("printInt",    ([Int     ], Void)),
     ("printDouble", ([Doub    ], Void)),
     ("printString", ([ConstStr], Void)),
     ("readInt",     ([        ], Int )),
     ("readDouble",  ([        ], Doub))]

extractFunIds :: ProgramA -> [FunId]
extractFunIds = map toFnSigId . progFuns

toFnSigId :: TopDefA -> FunId
toFnSigId (FnDef _ ret ident args _) =
    FunId ident $ FunSig (map argType args) ret