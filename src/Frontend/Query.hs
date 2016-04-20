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

import Control.Monad

import Frontend.Types

import Javalette.Abs

identStr :: Ident -> String
identStr (Ident i) = i

argToVar :: Arg a -> Var
argToVar (Arg _ typ ident) = Var ident $ void typ

argType :: Arg a -> Type a
argType (Arg _ t _) = t

itemIdent :: Item a -> Ident
itemIdent (Init _ i _) = i
itemIdent (NoInit _ i) = i

itemToVar :: Type a -> Item a -> Var
itemToVar typ = flip Var (void typ) . itemIdent

progFuns :: Program a -> [TopDef a]
progFuns (Program _ fns) = fns