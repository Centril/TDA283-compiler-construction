{- Javalette Compiler
   Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -}

{-|
Module      : Frontend.ParseLex
Description : Wrapper for Alex generated parser.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
Stability   : experimental
Portability : ALL

Wrapper for Alex generated parser.
-}
module Frontend.ParseLex (
    -- * Operations
    parseProgram
) where

import Javalette.Par
import Javalette.Abs
import qualified Javalette.ErrM as ErrM

import Frontend.Types

-- | 'parseProgram': parses a Javalette program into AST.
parseProgram :: String -> Err Program
parseProgram = translateErr . pProgram . myLexer

-- | translateErr: Converts from ErrM to 'Err'
translateErr :: ErrM.Err a -> Err a
translateErr (ErrM.Ok  o) = Right o
translateErr (ErrM.Bad b) = Left  b