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
    Env, FnSig, Sig, Context, Err, Log
) where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad

import Javalette.Lex
import Javalette.Par
import Javalette.Skel
import Javalette.Print
import Javalette.Abs

-- | 'Log': Logging type for errors in 'Err'.
type Log = String

-- | 'Err': Error type for potentially failing computations,
-- decorated with error log messages.
type Err = Either Log

-- | 'Env': Computational environment during compiler frontend passes.
type Env =     (Sig, [Context])

-- | 'FnSig': Signature of a function,
-- argument list (types) followed by return type.
type FnSig = ([Type], Type)

-- | 'Sig': Map of function identifiers -> signatures.
type Sig =     Map Ident FnSig

-- | Context: Context stack, map from variables -> types.
type Context = Map Ident Type