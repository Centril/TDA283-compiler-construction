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
module Frontend.Error where

import Frontend.Types
import Frontend.Query

import Javalette.Abs

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

wrongRetTyp :: Type -> Type -> Eval a
wrongRetTyp texpected tactual =
    terr' ["The current function expected return type:", show texpected,
                                           ", actual:", show tactual]

wrongExpTyp :: Expr -> Type -> Type -> Eval a
wrongExpTyp expr texpected tactual =
    terr' ["The expression", show expr,
          "expected the type:", show texpected, ",",
                "actual type:", show tactual]

wrongIdentTyp :: Ident -> [Type] -> Type -> Eval a
wrongIdentTyp ident types tactual =
    terr' ["The expression", identStr ident,
          "expected one of the types:", show types, ",",
                        "actual type:", show tactual]

wrongUnaryExp :: Expr -> [Type] -> Type -> Eval a
wrongUnaryExp expr types tactual =
    terr' ["The unary expression", show expr,
             "expected one of the types:", show types, ",",
                           "actual type:", show tactual]

wrongBinExp :: Expr -> Expr -> Type -> Type -> Eval a
wrongBinExp exprl exprr typl typr =
     terr' ["The binary expression has different types for operands:",
           "(", show exprl, ",", show typl, ")",
           "(", show exprr, ",", show typr, ")"]

wrongArgsTyp :: Ident -> [Type] -> [Type] -> Eval a
wrongArgsTyp ident texpected tactual =
    terr' ["The function ", identStr ident,
          "expected the types:", show texpected, ",",
          "but was applied with actual types:", show tactual]

insufficientFunRet :: Ident -> Eval a
insufficientFunRet fun = terr' ["The function", identStr fun, "might not return"]