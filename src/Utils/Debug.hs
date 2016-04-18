{- Javalette Compiler
   Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -}

{-|
Module      : Debug
Description : Debugging of stuff.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
Stability   : unsafe
Portability : ALL

Debugging of stuff.
-}
module Utils.Debug where

import System.IO.Unsafe

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty

prettify :: String -> String
prettify str = case parseExp str of
    ParseOk res -> prettyPrintStyleMode style
        {lineLength = 120, ribbonsPerLine = 1.2} defaultMode res
    ParseFailed{} -> str

-- | 'debug': this function is morally dubious to use,
-- it lacks referential transparency and might destroy your computer,
-- set your house on fire, create black holes,
-- without it saying so in the type signature.
debug :: Show a => a -> a
debug x = unsafePerformIO $ (putStrLn $ prettify $ show x) >> return x