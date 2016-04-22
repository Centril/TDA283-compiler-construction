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
Module      : Frontend.TypeInfer
Description : Type inference for Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Type inference for Javalette compiler.
-}
{-# LANGUAGE TupleSections #-}

module Frontend.TypeInfer (
    -- * Operations
    inferExp
) where

import Control.Monad
import Control.Arrow

import Javalette.Abs

import Frontend.Annotations
import Frontend.Environment
import Frontend.Computation
import Frontend.Error
import Frontend.Common

appConcrete :: (ASTAnots -> TypeA) -> TypeA
appConcrete typ = typ [AKind KConcrete]

conststr :: TypeA
conststr = appConcrete ConstStr

int :: TypeA
int = appConcrete Int

doub :: TypeA
doub = appConcrete Doub

bool :: TypeA
bool = appConcrete Bool

tAnot :: ASTAnots -> TypeA -> ASTAnots
tAnot a typ = a ++ [AType typ]

simpleAnot :: ExprA -> TypeA -> (ExprA, TypeA)
simpleAnot expr typ = (flip tAnot typ <$> expr, typ)

flip3 :: (a -> b -> c -> r) -> c -> a -> b -> r
flip3 f c a b = f a b c

--------------------------------------------------------------------------------
-- Type inference:
--------------------------------------------------------------------------------

inferExp :: ExprA -> Eval (ExprA, TypeA)
inferExp expr = case expr of
    EVar _ ident   -> simpleAnot expr <$> lookupVarE ident
    EString   _ _  -> return $ simpleAnot expr conststr
    ELitInt   _ _  -> return $ simpleAnot expr int
    ELitDoub  _ _  -> return $ simpleAnot expr doub
    ELitTrue  _    -> return $ simpleAnot expr bool
    ELitFalse _    -> return $ simpleAnot expr bool
    EApp a ident e -> inferFun a ident e
    Neg a e        -> inferUnary Neg a e [int, doub]
    Not a e        -> inferUnary Not a e [bool]

    EMul a l op r  -> inferBin l r (mulOp op)  (emul op) (tAnot a)
    EAdd a l op r  -> inferBin l r [int, doub] (eadd op) (tAnot a)
    ERel a l op r  -> inferBin l r (relOp op)  (erel op) (const $ tAnot a bool)
    EAnd a l r     -> inferBin l r [bool]      EAnd (tAnot a)
    EOr  a l r     -> inferBin l r [bool]      EOr (tAnot a)

emul = flip3 EMul
eadd = flip3 EAdd
erel = flip3 ERel

relOp :: RelOpA -> [TypeA]
relOp oper | oper `elem` applyEA [NE, EQU] = [int, doub, bool]
           | otherwise                     = [int, doub]

mulOp :: MulOpA -> [TypeA]
mulOp oper | oper == Mod emptyAnot = [int]
           | otherwise             = [int, doub]

inferBin exprl exprr accept ctor anot = do
    (exprl', typl) <- inferExp exprl
    (exprr', typr) <- inferExp exprr
    if typl == typr && typl `elem` accept
        then return (ctor (anot typl) exprl' exprr', typl)
        else wrongBinExp exprl exprr typl typr

inferUnary :: (ASTAnots -> ExprA -> ExprA)
           -> ASTAnots -> ExprA -> [TypeA]
           -> Eval (ExprA, TypeA)
inferUnary ctor a expr accept = do
    r@(expr', etyp) <- inferExp expr
    if etyp `elem` accept
        then return (ctor (tAnot a etyp) expr', etyp)
        else wrongUnaryExp expr accept etyp

inferFun :: ASTAnots -> Ident -> [ExprA] -> Eval (ExprA, TypeA)
inferFun a ident exprs = do
    FunSig texpected rtype <- lookupFunE ident
    (exprs', tactual)      <- mapAndUnzipM inferExp exprs
    if texpected == tactual
        then return (EApp (tAnot a rtype) ident exprs', rtype)
        else wrongArgsTyp ident texpected tactual