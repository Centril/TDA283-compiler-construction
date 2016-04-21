{- Javalette Compiler, a simple C like language.
 - Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -
 - This program is free software; you can redistribute it and/or
 - modify it under the terms of the GNU General Public License
 - as published by the Free Software Foundation; either version 2
 - of the License, or (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program; if not, write to the Free Software
 - Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 -}

{-|
Module      : Frontend.Query
Description : Querying functions on AST in Javalette compiler frontend.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Querying functions on AST in Javalette compiler frontend
-}
module Frontend.Query where

import Javalette.Abs

import Frontend.Annotations
import Frontend.Types

identStr :: Ident -> String
identStr (Ident i) = i

argToVar :: ArgA -> Var
argToVar (Arg _ typ ident) = Var ident typ

argType :: ArgA -> TypeA
argType (Arg _ t _) = t

itemIdent :: ItemA -> Ident
itemIdent (Init _ i _) = i
itemIdent (NoInit _ i) = i

itemToVar :: TypeA -> ItemA -> Var
itemToVar typ = flip Var typ . itemIdent

progFuns :: ProgramA -> [TopDefA]
progFuns (Program _ fns) = fns