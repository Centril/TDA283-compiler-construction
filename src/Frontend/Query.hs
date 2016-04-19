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
module Frontend.Query where

import Frontend.Types

import Javalette.Abs

identStr :: Ident -> String
identStr (Ident i) = i

argType :: Arg -> Type
argType (Arg t _) = t

progFuns :: Program -> [TopDef]
progFuns (Program fns) = fns

toFnSigId' :: TopDef -> FunId
toFnSigId' = undefined -- (FnDef ret ident args _) (ident, (map argType args, ret))