{- Javalette Compiler
   Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -}

{-|
Module      : Debug
Description : Error messsages.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
Stability   : unsafe
Portability : ALL

Error messages.
-}
module Utils.Error where

import Javalette.Abs

wrgFunSig :: Ident -> String
wrgFunSig fun =
    unwords ["The function", show fun, "has the wrong signature."]

funcAlrDef :: Ident -> String
funcAlrDef fun =
    unwords ["The function", show fun, "is already defined."]

funcNotDef :: Ident -> String
funcNotDef fun =
    unwords ["The function", show fun, "is not defined."]

varAlrDef :: Ident -> String
varAlrDef var =
    unwords ["The variable/parameter", show var, "is already defined."]

varNotDef :: Ident -> String
varNotDef var =
    unwords ["The variable/parameter", show var, "is not defined."]

wrgBinExp :: Expr -> Expr -> Type -> Type -> String
wrgBinExp exp1 exp2 typ1 typ2 =
     unwords ["The binary expression", "(", show exp1, ",", show typ1, ")",
              "(", show exp2, ",", show typ2, ")", "has different types."]

wrgRelExp :: Expr -> Expr -> Type -> Type -> String
wrgRelExp exp1 exp2 typ1 typ2 =
    unwords ["The relation expression", "(", show exp1, ",", show typ1, ")",
             "(", show exp2, ",", show typ2, ")", "has different types."]

wrgUnaExp :: Expr -> Type -> String
wrgUnaExp expr typ =
    unwords ["The unary expression", "(", show expr, ",", show typ, ")",
             "has the wrong type."]

wrgArgTyp :: Ident -> [Type] -> [Type] -> String
wrgArgTyp ident ty1s ty2s =
    unwords ["The function", show ident, "expected the types", show ty1s,
             "but got the types", show ty2s]

wrgExpTyp :: Expr -> Type -> Type -> String
wrgExpTyp expr typ1 typ2 =
    unwords ["The expression", show expr, "expected the type", show typ1,
             "but got the type", show typ2]

wrgIdeTyp :: Ident -> Type -> Type -> String
wrgIdeTyp ident typ1 typ2 =
    unwords ["The expression", show ident, "expected the type", show typ1,
             "but got the type", show typ2]

wrgIdeTyp' :: Ident -> [Type] -> Type -> String
wrgIdeTyp' ident types typ =
    unwords ["The expression", show ident, "expected one of the types",
             show types, "but got the type", show typ]

wrgVoidTyp :: Type -> String
wrgVoidTyp typ =
    unwords ["The current function expected return type", show Void,
             "but got the type", show typ]
