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
Module      : Common.StateOps
Description : Common MonadState operations using lenses in Javalette Compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Common MonadState operations using lenses in Javalette Compiler.
-}
{-# LANGUAGE FlexibleContexts #-}

module Common.StateOps (
    -- * Operations
    postInc, freshOf, sPopM, sPushM, sAppendL, ctxFirst
) where

import Prelude hiding (lookup)

import Safe

import Data.Maybe
import Data.Map (Map, empty, lookup)

import Control.Arrow
import Control.Monad.State
import Control.Lens

import Utils.Foldable

type Update s r = Over (->) ((,) r) s s r r

postInc :: (Num r, MonadState s m) => Update s r -> m r
postInc = (%%= (id &&& (+1)))

freshOf :: (Num a, Show a, MonadState s f) => String -> Update s a -> f String
freshOf prefix = fmap ((prefix ++) . show) . postInc

sPopM, sPushM :: MonadState s m => ASetter s s [Map k a] [Map k a] -> m ()
sPushM = (%= (empty:))
sPopM  = (%= (fromMaybe [] . tailMay))

sAppendL :: (Applicative f, Monoid (f a), MonadState s m)
         => ASetter s s (f a) (f a) -> a -> m ()
sAppendL x l = x %= (`mappend` pure l)

ctxFirst :: (Ord k, Foldable f) => k -> f (Map k v) -> Maybe v
ctxFirst = mfind . lookup