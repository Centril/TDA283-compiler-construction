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
{-# LANGUAGE LambdaCase#-}

module Frontend.TypeFunSig (
    -- * Operations
    mainCorrect, allFunctions
) where

import Control.Monad

import Control.Lens

import Utils.Monad

import Common.AST
import Common.ASTOps

import Frontend.Environment
import Frontend.Error
import Frontend.Common
import Frontend.TypeInfer

--------------------------------------------------------------------------------
-- Checking for int main(void):
--------------------------------------------------------------------------------

mainId :: Ident
mainId = Ident "main"

mainCorrect :: TCComp ()
mainCorrect = lookupFunE mainId >>=
    flip unless wrongMainSig . (== FunSig [] int)

--------------------------------------------------------------------------------
-- Collecting function signatures:
--------------------------------------------------------------------------------

allFunctions :: ProgramA -> TCComp ProgramA
allFunctions = pTopDefs %%~
    (<<= collectFuns . (predefFuns ++) . (sndsOfPrism _TFnDef >$> toFnSigIds)) .
    mapM (toFnDef %%~ checkFunSignature)

checkFunSignature :: FnDefA -> TCComp FnDefA
checkFunSignature fun = do
    args' <- mapM checkArgTypes $ _fArgs fun
    (ret',  _) <- inferType $ _fRetTyp fun
    return $ fun { _fRetTyp = ret', _fArgs = args' }

checkArgTypes :: ArgA -> TCComp ArgA
checkArgTypes = aTyp %%~ (fmap fst . inferType)

collectFuns :: [FunId] -> TCComp ()
collectFuns = mapM_ $ extendFun' funAlreadyDef

predefFuns :: [FunId]
predefFuns = toFunId <$>
    [("printInt",    ([int     ], tvoid)),
     ("printDouble", ([doub    ], tvoid)),
     ("printString", ([conststr], tvoid)),
     ("readInt",     ([        ], int )),
     ("readDouble",  ([        ], doub))]

toFnSigIds :: FnDefA -> FunId
toFnSigIds (FnDef _ ret name args _) = FunId name $ FunSig (_aTyp <$> args) ret