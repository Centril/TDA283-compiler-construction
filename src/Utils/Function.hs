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
Module      : Utils.Function
Description : General utility for functions and arrows.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

General utility for functions and arrows.
-}
module Utils.Function (
    -- * Operations
    flip3, (§), (<§>)
) where

-- | 'flip3': move the third argument of a function to the front.
flip3 :: (a -> b -> c -> r) -> c -> a -> b -> r
flip3 f c a b = f a b c

-- | '(§)': apply a function to both elements of pair.
(§) :: (a -> b) -> (a, a) -> (b, b)
(§) f (a, b) = (f a, f b)

-- | '(<§>)': fan out, apply each element in
-- a pair of functions to a pair of values.
(<§>) :: (a -> b, c -> d) -> (a, c) -> (b, d)
(<§>) (f, g) (a, b) = (f a, g b)