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
Module      : Frontend.Error
Description : Error messsages in Frontend of Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : unsafe
Portability : ALL

Error messsages in Frontend of Javalette compiler.
-}
module Frontend.Error where

import Frontend.Computation
import Frontend.Query

import Common.AST

terr :: String -> Eval a
terr = err TypeChecker

terr' :: [String] -> Eval a
terr' = err' TypeChecker

xAlreadyDef :: String -> Ident -> Eval a
xAlreadyDef what x = terr' ["The", what, identStr x, "is already defined"]

xNotDef :: String -> Ident -> Eval a
xNotDef what x = terr' ["The", what, identStr x, " is not defined."]

wrongMainSig :: Eval a
wrongMainSig = terr "The function: main has the wrong signature"

funNotDef, varNotDef :: Ident -> Eval a
funNotDef = xNotDef "function"
varNotDef = xNotDef "variable or parameter"

funAlreadyDef, argAlreadyDef, varAlreadyDef :: Ident -> Eval a
funAlreadyDef = xAlreadyDef "function"
argAlreadyDef = xAlreadyDef "parameter"
varAlreadyDef = xAlreadyDef "variable"

wrongRetTyp :: Show b => Type b -> Type b -> Eval a
wrongRetTyp texpected tactual =
    terr' ["The current function expected return type:", show texpected,
                                           ", actual:", show tactual]

wrongExpTyp :: Show b =>  Expr b -> Type b -> Type b -> Eval a
wrongExpTyp expr texpected tactual =
    terr' ["The expression", show expr,
          "expected the type:", show texpected, ",",
                "actual type:", show tactual]

wrongIdentTyp :: Show b => Ident -> [Type b] -> Type b -> Eval a
wrongIdentTyp ident types tactual =
    terr' ["The expression", identStr ident,
          "expected one of the types:", show types, ",",
                        "actual type:", show tactual]

wrongUnaryExp :: Show b => Expr b -> [Type b] -> Type b -> Eval a
wrongUnaryExp expr types tactual =
    terr' ["The unary expression", show expr,
             "expected one of the types:", show types, ",",
                           "actual type:", show tactual]

wrongBinExp :: Show b => Expr b -> Expr b -> Type b -> Type b -> Eval a
wrongBinExp exprl exprr typl typr =
     terr' ["The binary expression has different types for operands:",
           "(", show exprl, ",", show typl, ")",
           "(", show exprr, ",", show typr, ")"]

wrongArgsTyp :: Show b =>  Ident -> [Type b] -> [Type b] -> Eval a
wrongArgsTyp ident texpected tactual =
    terr' ["The function ", identStr ident,
          "expected the types:", show texpected, ",",
          "but was applied with actual types:", show tactual]

insufficientFunRet :: Ident -> Eval a
insufficientFunRet fun =
    terr' ["The function", identStr fun, "might not return"]