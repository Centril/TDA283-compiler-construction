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
Module      : Utils.Pointless
Description : Combinators to ease writing common cases in pointfree style.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Combinators to ease writing common cases in pointfree style.
-}
module Utils.Pointless (
    -- * Operations
    (.|), (|.|), (|.)
) where

-- | '.|': Compose an unary function with a binary function.
-- from: http://hackage.haskell.org/package/pointless-fun-1.1.0.5/docs/Data-Function-Pointless.html
(.|) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.|) f g x y = f $ g x y
infixr 7 .|

-- | '|.|': Compose a binary function with two unary functions.
(|.|) :: (c -> d -> e) -> ((a -> c), (b -> d)) -> a -> b -> e
(|.|) f (g, h) a b = f (g a) (h b)
infixr 8 |.|

-- | '|.': Compose a binary function with an unary function for both arguments.
(|.) :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(|.) f g a b = f (g a) (g b)
infixr 8 |.