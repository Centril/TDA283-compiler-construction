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
Module      : Utils.Debug
Description : Debugging of stuff.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : unsafe
Portability : ALL

Debugging of stuff.
-}
module Utils.Debug (
    -- * Operations
    debug
) where

import System.IO.Unsafe

import Utils.Terminal

-- | 'debug': this function is morally dubious to use,
-- it lacks referential transparency and might destroy your computer,
-- set your house on fire, create black holes,
-- without it saying so in the type signature.
debug :: Show a => a -> a
debug x = unsafePerformIO $ putStrLn (prettify $ show x) >> return x