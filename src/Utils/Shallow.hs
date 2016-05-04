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
Module      : Utils.Shallow
Description : Type classes for shallow mapping and extraction.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Type classes for shallow mapping and extraction.
-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}

module Utils.Shallow where

import Control.Lens (over, view)

-- | 'Overable': used for types that can be used for the 'over' function
-- from Control.Lens.
class Overable o where
    overF :: Functor f => (a0 -> f a0) -> o a0 -> f (o a0)

-- | 'Shallowable': used for types that can be shallowly mapped over.
-- The functor laws also apply to this class.
class Shallowable sm where
    -- | 'smap': maps the immediate value of the given 'Shallowable' and only
    -- of that node. Unlike 'fmap', it does not map all subnodes, and the type
    -- of the mapped value must also be the same before and after.
    smap :: (a -> a) -> sm a -> sm a

-- | '(<@>)': infix version of 'smap'.
(<@>) :: Shallowable f => (a -> a) -> f a -> f a
(<@>) = smap

-- | 'Extractable': exposes the immediate value of some structure.
class Extractable e where
    -- | 'extract': extracts the value held in data structure.
    extract :: e a -> a

instance Overable o => Shallowable o where smap    = over overF
instance Overable o => Extractable o where extract = view overF