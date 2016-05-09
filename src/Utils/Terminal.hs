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
Module      : Utils.Terminal
Description : Terminal related functions.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : unsafe
Portability : ALL

Terminal related functions.
-}
module Utils.Terminal (
    -- * Operations
    errStr, errLn, errChar, errPrint,
    poutput, prettify,
) where

import System.IO

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty

errStr, errLn :: String -> IO ()
errStr = hPutStr   stderr
errLn  = hPutStrLn stderr

errChar :: Char -> IO ()
errChar = hPutChar stderr

errPrint :: Show a => a -> IO ()
errPrint = hPrint  stderr

poutput :: Show a => a -> IO ()
poutput = putStrLn . prettify . show

prettify :: String -> String
prettify str = case parseExp str of
    ParseOk res -> prettyPrintStyleMode style
        {lineLength = 120, ribbonsPerLine = 1.2} defaultMode res
    ParseFailed{} -> str