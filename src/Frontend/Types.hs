{- Javalette Compiler
   Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -}

{-|
Module      : Frontend.Types
Description : Types for Frontend of Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
Stability   : experimental
Portability : ALL

Types for Frontend of Javalette compiler.
-}
module Frontend.Types (
    -- * Types
    Env, Sig, Context, Err, Log
) where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad

import Javalette.Lex
import Javalette.Par
import Javalette.Skel
import Javalette.Print
import Javalette.Abs

type Log = String
type Err = Either Log

-- TODO: Replace Err with Either
type Env =     (Sig, [Context])         -- functions and context stack
type Sig =     Map Ident ([Type], Type) -- function type signature
type Context = Map Ident Type           -- variables with their types