{- Javalette Compiler
   Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -}

{-|
Module      : Error
Description : Error messsages in Frontend of Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
Stability   : unsafe
Portability : ALL

Error messsages in Frontend of Javalette compiler.
-}
module Frontend.Error2 where

import Frontend.Types
import Frontend.Query

import Javalette.Abs

funNotDef :: Ident -> Eval a
funNotDef fun = err' ["The function:", identStr fun, " is not defined."]

wrongMainSig :: Eval a
wrongMainSig = err "The function: main has the wrong signature"

funAlreadyDef :: Ident -> Eval a
funAlreadyDef fun = err' ["The function", identStr fun, "is already defined"]