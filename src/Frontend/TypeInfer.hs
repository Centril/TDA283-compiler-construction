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
    inferExp,
    lookupFunE, lookupVarE
) where

import Control.Monad
import Control.Arrow

import Javalette.Abs

import Frontend.Types
import Frontend.Error

--------------------------------------------------------------------------------
-- Type inference:
--------------------------------------------------------------------------------

inferExp :: ExprA -> Eval (ExprA, TypeA)
inferExp expr = case expr of
    EVar a ident   -> (EVar a ident,) <$> lookupVarE ident
    EString a  v   -> return (EString a v, ConstStr a)
    ELitInt a v    -> return (ELitInt a v, Int a     )
    ELitDoub a v   -> return (ELitDoub a v, Doub a   )
    ELitTrue a     -> return (ELitTrue a  , Bool a   )
    ELitFalse a    -> return (ELitFalse a , Bool a   )
    EApp a ident e -> first (EApp a ident) <$> inferFun ident e
    Neg a e        -> inferUnary [Int a, Doub a] e
    Not a e        -> inferUnary [Bool a] e
    EMul a l op r  -> inferBin (makeBin EMul a op) (mulOp op)  l r
    EAdd a l op r  -> inferBin (makeBin EAdd a op) [Int a, Doub a] l r
    ERel a l op r  -> second (const $ Bool a) <$>
                      inferBin (makeBin ERel a op) (relOp op) l r
    EAnd a l r     -> inferBin (EAnd a)    [Bool a]    l r
    EOr  a l r     -> inferBin (EOr a)     [Bool a]    l r

makeBin :: (t -> a -> b -> c) -> t -> b -> a -> c
makeBin f a = flip (f a)

inferBin :: (ExprA -> ExprA -> ExprA)
         -> [TypeA]
         -> ExprA
         -> ExprA
         -> Eval (ExprA, TypeA)
inferBin op accept le re = first (uncurry op) <$> inferBinary accept le re

relOp :: RelOpA -> [TypeA]
relOp oper | oper `elem` applyEA [NE, EQU] = applyEA [Int, Doub, Bool]
           | otherwise                     = applyEA [Int, Doub]

mulOp :: MulOpA -> [TypeA]
mulOp oper | oper == Mod emptyAnot = applyEA [Int]
           | otherwise             = applyEA [Int, Doub]

inferBinary :: [TypeA] -> ExprA -> ExprA -> Eval ((ExprA, ExprA), TypeA)
inferBinary types exprl exprr = do
    (exprl', typl) <- inferExp exprl
    (exprr', typr) <- inferExp exprr
    if typl == typr && typl `elem` types
        then return ((exprl', exprr'), typl)
        else wrongBinExp exprl exprr typl typr

inferUnary :: [TypeA] -> ExprA -> Eval (ExprA, TypeA)
inferUnary types expr = do
    r@(_, etyp) <- inferExp expr
    if etyp `elem` types
        then return r
        else wrongUnaryExp expr types etyp

inferFun :: Ident -> [ExprA] -> Eval ([ExprA], TypeA)
inferFun ident exprs = do
    FunSig texpected rtype <- lookupFunE ident
    (exprs', tactual)      <- mapAndUnzipM inferExp exprs
    if texpected == tactual
        then return (exprs', rtype)
        else wrongArgsTyp ident texpected tactual

--------------------------------------------------------------------------------
-- Lookups:
--------------------------------------------------------------------------------

lookupFunE :: Ident -> Eval FunSig
lookupFunE = lookupFun' funNotDef

lookupVarE :: Ident -> Eval TypeA
lookupVarE = lookupVar' varNotDef