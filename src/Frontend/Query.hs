{- Javalette Compiler
   Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -}

{-|
Module      : Frontend.Query
Description : Querying functions on AST in Javalette compiler frontend.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
Stability   : experimental
Portability : ALL

Querying functions on AST in Javalette compiler frontend
-}
module Frontend.Query (
    -- * Operations
    argType, progFuns
) where

import Javalette.Abs

argType :: Arg -> Type
argType (Arg t i) = t

progFuns :: Program -> [TopDef]
progFuns (Program fns) = fns