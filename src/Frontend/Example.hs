{- Javalette Compiler
   Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -}

{-|
Module      : Frontend.Example
Description : Type checker for Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
Stability   : experimental
Portability : ALL

Example function for the typechecker.
-}
module Frontend.Example (
    -- * Types
    Env, Sig, Context, Err, Log,

    -- * Operations
    simpleBlock
) where

import Javalette.Abs

import Frontend.Types

simpleBlock :: Block
simpleBlock = Block [Decl Int [Init (Ident "i")
    (ELitInt 1)],Ret (EVar (Ident "i"))]
