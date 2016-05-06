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

import Control.Lens

import Utils.Monad

import Common.AST

import Frontend.Environment
import Frontend.Error
import Frontend.Common
import Frontend.TypeInfer

--------------------------------------------------------------------------------
-- Checking for int main(void):
--------------------------------------------------------------------------------

mainId :: Ident
mainId = Ident "main"

mainCorrect :: Eval ()
mainCorrect = lookupFunE mainId >>=
    flip unless wrongMainSig . (== FunSig [] int)

--------------------------------------------------------------------------------
-- Collecting function signatures:
--------------------------------------------------------------------------------

allFunctions :: ProgramA -> Eval ProgramA
allFunctions = pTopDefs %%~ (<<= collectFuns . (predefFuns ++) . fmap toFnSigId)
                            . mapM checkFunSignature

checkFunSignature :: TopDef ASTAnots -> Eval TopDefA
checkFunSignature fun = do args'      <- mapM checkArgTypes $ _fArgs fun
                           (ret',  _) <- inferType $ _fRetTyp fun
                           return $ fun { _fRetTyp = ret', _fArgs = args' } 

checkArgTypes :: Arg ASTAnots -> Eval ArgA
checkArgTypes = aTyp %%~ (fmap fst . inferType)

collectFuns :: [FunId] -> Eval ()
collectFuns = mapM_ $ extendFun' funAlreadyDef

predefFuns :: [FunId]
predefFuns = map toFunId
    [("printInt",    ([int     ], tvoid)),
     ("printDouble", ([doub    ], tvoid)),
     ("printString", ([conststr], tvoid)),
     ("readInt",     ([        ], int )),
     ("readDouble",  ([        ], doub))]

toFnSigId :: TopDefA -> FunId
toFnSigId (FnDef _ ret name args _) = FunId name $ FunSig (_aTyp <$> args) ret