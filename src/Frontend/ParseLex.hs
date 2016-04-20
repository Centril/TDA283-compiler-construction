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
import Javalette.ErrM

import Frontend.Types

import Debug.Trace
import Utils.Debug

-- | 'parseProgram': statefully parses a Javalette program into AST.
parseProgram :: String -> Eval Program
parseProgram code = do
    infoln Parser ["Attempting to parse with:", code]
    case debug $ pProgram $ myLexer code of
        Ok  ast -> info' Parser ["Successfully parsed!"] >> return ast
        Bad msg -> info  Parser "Parse error!"           >> err Parser msg