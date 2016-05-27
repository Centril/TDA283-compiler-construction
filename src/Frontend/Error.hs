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

import Data.List

import Frontend.Environment

import Common.AST

joinComma :: [String] -> String
joinComma = intercalate ", "

terr :: String -> TCComp a
terr = err TypeChecker

terr' :: [String] -> TCComp a
terr' = err' TypeChecker

xAlreadyDef :: String -> Ident -> TCComp a
xAlreadyDef what x = terr' ["The", what, _ident x, "is already defined."]

xNotDef :: String -> Ident -> TCComp a
xNotDef what x = terr' ["The", what, _ident x, " is not defined."]

wrongMainSig :: TCComp a
wrongMainSig = terr "The function: main has the wrong signature."

funNotDef, varNotDef :: Ident -> TCComp a
funNotDef = xNotDef "function"
varNotDef = xNotDef "variable or parameter"

funAlreadyDef, argAlreadyDef, varAlreadyDef :: Ident -> TCComp a
funAlreadyDef = xAlreadyDef "function"
argAlreadyDef = xAlreadyDef "parameter"
varAlreadyDef = xAlreadyDef "variable"

unusedVar :: Var -> TCComp ()
unusedVar var = warn' TypeChecker
    ["The", showVS $ vsource var, _ident $ vident var, "was unused."]

wrongRetTyp :: Show b => Type b -> Type b -> TCComp a
wrongRetTyp texpected tactual =
    terr' ["The current function expected return type:", show texpected,
                                            ", actual:", show tactual]

wrongExpTyp :: Show b =>  Expr b -> Type b -> Type b -> TCComp a
wrongExpTyp expr texpected tactual =
    terr' ["The expression", show expr,
          "expected the type:", show texpected, ",",
                "actual type:", show tactual]

wrongExprTyp :: Show b => ExprA -> [Type b] -> Type b -> TCComp a
wrongExprTyp lval types tactual =
    terr' ["The expression", show lval,
           "was expected to be one of the types:", show types, ",",
                                   "actual type:", show tactual]

wrongUnaryExp :: Show b => Expr b -> [Type b] -> Type b -> TCComp a
wrongUnaryExp expr types tactual =
    terr' ["The unary expression", show expr,
             "expected one of the types:", show types, ",",
                           "actual type:", show tactual]

wrongBinExp :: Show b => Expr b -> Expr b -> Type b -> Type b -> TCComp a
wrongBinExp exprl exprr typl typr =
     terr' ["The binary expression has different types for operands:",
           "(", show exprl, ",", show typl, ")",
           "(", show exprr, ",", show typr, ")"]

wrongArgsTyp :: Show b =>  Ident -> [Type b] -> [Type b] -> TCComp a
wrongArgsTyp fun texpected tactual =
    terr' ["The function ", _ident fun,
          "expected the types:", show texpected, ",",
          "but was applied with actual types:", show tactual]

insufficientFunRet :: Ident -> TCComp a
insufficientFunRet fun = terr' ["The function", _ident fun, "might not return."]

propNotExists :: Ident -> TypeA -> TCComp a
propNotExists prop typ = 
    terr' ["The type", show typ, "does not have a property", _ident prop ++ "."]

arrayNotStruct :: TypeA -> TCComp a
arrayNotStruct typ = terr' ["The array type", show typ, "is not a struct."]

primNoAccProps :: TypeA -> TCComp a
primNoAccProps typ = terr' ["The type", show typ, "has no accessable properties."]

accArrOverDimen :: Ident -> Int -> Int -> TCComp a
accArrOverDimen name arr attempt =
    terr' ["Illegal access element of array", _ident name ,
           "with dimensions", show arr, "in", show attempt,
           "levels deep, which does not yield a type."]

accOfNotArr :: Ident -> TypeA -> TCComp a
accOfNotArr name typ = terr' ["Illegal attempt to access", _ident name,
                              "of type: ", show typ, "as an array."]

accArrNotInt :: TypeA -> TCComp a
accArrNotInt typ = terr' ["Illegal use of non-integer type", show typ,
                          "in array element access."]

typeNotNewable :: TypeA -> TCComp a
typeNotNewable typ =
    terr' ["Illegal use of base type", show typ,
           "in new, which is not a primitive type."]

nullNotCastable :: TypeA -> TCComp a
nullNotCastable typ =
    terr' ["Illegal attempt to cast null to the type", show typ,
           ", which is not a pointer type."]

noSuchTypeName :: Ident -> TCComp a
noSuchTypeName alias = terr' ["The type", _ident alias, "is not defined."]

typeIsReserved :: Ident -> TypeA -> TCComp a
typeIsReserved name typ =
    terr' ["Type name", _ident name,
           "has already been defined as", show typ ++ "."]

structDupFields :: Ident -> [SFieldA] -> [SFieldA] -> TCComp a
structDupFields name _ dups =
    terr' ["Struct", _ident name, "has duplicate fields:",
           joinComma (_ident . _sfIdent <$> dups) ++ "."]