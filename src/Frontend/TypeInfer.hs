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
    inferExp, inferType
) where

import Control.Monad

import Utils.Function

import Frontend.Computation
import Frontend.Error
import Frontend.Common

import Javalette.Abs

--------------------------------------------------------------------------------
-- Kind inference:
--------------------------------------------------------------------------------

-- @TODO: For now, all types have a concrete kind, change this?
inferType :: TypeA -> Eval (TypeA, Kind)
inferType typ = return $ addKind typ KConcrete

--------------------------------------------------------------------------------
-- Type inference:
--------------------------------------------------------------------------------

inferExp :: ExprA -> Eval (ExprA, TypeA)
inferExp expr = case expr of
    EVar _ ident   -> addTyp  expr <$> lookupVarE ident
    EString   {}   -> addTyp' expr conststr
    ELitInt   {}   -> addTyp' expr int
    ELitDoub  {}   -> addTyp' expr doub
    ELitTrue  _    -> addTyp' expr bool
    ELitFalse _    -> addTyp' expr bool
    EApp a ident e -> inferFun a ident e
    Neg a e        -> inferUnary Neg a e [int, doub]
    Not a e        -> inferUnary Not a e [bool]
    EMul a l op r  -> inferBin a l r (mulOp op)  (flip3 EMul op) id
    EAdd a l op r  -> inferBin a l r [int, doub] (flip3 EAdd op) id
    ERel a l op r  -> inferBin a l r (relOp op)  (flip3 ERel op) (const bool)
    EAnd a l r     -> inferBin a l r [bool]             EAnd     id
    EOr  a l r     -> inferBin a l r [bool]             EOr      id

relOp :: RelOpA -> [TypeA]
relOp oper | oper `elem` applyEA [NE, EQU] = [int, doub, bool]
           | otherwise                     = [int, doub]

mulOp :: MulOpA -> [TypeA]
mulOp oper | oper == Mod emptyAnot = [int]
           | otherwise             = [int, doub]

inferBin :: ASTAnots -> ExprA -> ExprA -> [TypeA]
         -> (ASTAnots -> ExprA -> ExprA -> ExprA) -> (TypeA -> TypeA)
         -> Eval (ExprA, TypeA)
inferBin a exprl exprr accept ctor mTyp = do
    (exprl', typl) <- inferExp exprl
    (exprr', typr) <- inferExp exprr
    if typl == typr && typl `elem` accept
        then addTyp' (ctor a exprl' exprr') (mTyp typl)
        else wrongBinExp exprl exprr typl typr

inferUnary :: (ASTAnots -> ExprA -> ExprA)
           -> ASTAnots -> ExprA -> [TypeA]
           -> Eval (ExprA, TypeA)
inferUnary ctor a expr accept = do
    (expr', etyp) <- inferExp expr
    if etyp `elem` accept
        then addTyp' (ctor a expr') etyp
        else wrongUnaryExp expr accept etyp

inferFun :: ASTAnots -> Ident -> [ExprA] -> Eval (ExprA, TypeA)
inferFun a ident exprs = do
    FunSig texpected rtype <- lookupFunE ident
    (exprs', tactual)      <- mapAndUnzipM inferExp exprs
    if texpected == tactual
        then addTyp' (EApp a ident exprs') rtype
        else wrongArgsTyp ident texpected tactual