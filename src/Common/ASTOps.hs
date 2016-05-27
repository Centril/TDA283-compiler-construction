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
Module      : Common.ASTOps
Description : Common AST operations Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Common AST operations Javalette compiler.
-}
module Common.ASTOps (
    -- * Operations
    progCollect, intoProg, sndsOfPrism
) where

import Data.Monoid (First)
import Data.Maybe

import Control.Monad

import Control.Lens

import Utils.Pointless
import Utils.Monad

import Common.AST
import Common.Annotations

progCollect :: Monad m => Getting (First (x, a)) TopDefA (x, a)
            -> (a -> m b) -> ProgramA -> m ()
progCollect prism_ = void .| intoProg prism_

intoProg :: Monad m => Getting (First (x, a)) TopDefA (x, a)
            -> (a -> m b) -> ProgramA -> m [b]
intoProg prism_ f prog = mapM f $ sndsOfPrism prism_ $ _pTopDefs prog

sndsOfPrism :: Getting (First (x, b)) a (x, b) -> [a] -> [b]
sndsOfPrism prism_ = catMaybes . fmap ((^? prism_) >$> snd)