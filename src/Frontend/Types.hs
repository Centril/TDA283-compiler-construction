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

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Frontend.Types (
    -- * Types
    Env, FnSig, FnSigId, Sig, Context, Err, Log
) where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad

import Data.Data
import Data.Generics.Uniplate.Data

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

-- | 'Ident': Identifier of a function,
-- | 'FnSig': Signature of a function,
--  a simpler representation in regards to Sig
type FnSigId = (Ident, FnSig)

-- | 'Sig': Map of function identifiers -> signatures.
type Sig =     Map Ident FnSig

-- | Context: Context stack, map from variables -> types.
type Context = Map Ident Type

deriving instance Data Ident
deriving instance Typeable Ident

deriving instance Data Type
deriving instance Typeable Type

deriving instance Data Arg
deriving instance Typeable Arg