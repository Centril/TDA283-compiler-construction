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
{-# LANGUAGE TupleSections, LambdaCase #-}

module Frontend.TypeInfer (
    -- * Operations
    inferExp, inferType
) where

import Control.Monad

import Control.Lens

import Utils.Monad

import Common.AST

import Frontend.Environment
import Frontend.Error
import Frontend.Common

--------------------------------------------------------------------------------
-- Kind inference:
--------------------------------------------------------------------------------

-- TODO: For now, all types have a concrete kind, change this?
inferType :: TypeA -> TCComp (TypeA, Kind)
inferType typ = return $ addKind typ KConcrete

--------------------------------------------------------------------------------
-- Type inference:
--------------------------------------------------------------------------------

inferExp :: ExprA -> TCComp (ExprA, TypeA)
inferExp expr = case expr of
    ENew      {} -> inferENew expr
    EVar _ n  [] -> inferEVar n expr
    EVar _ n  ds -> inferEVar' n expr ds
    Length    {} -> inferLength expr
    EString   {} -> addTyp' expr conststr
    ELitInt   {} -> addTyp' expr int
    ELitDoub  {} -> addTyp' expr doub
    ELitTrue  _  -> addTyp' expr bool
    ELitFalse _  -> addTyp' expr bool
    EApp      {} -> inferFun expr
    Neg       {} -> inferUnary expr [int, doub]
    Not       {} -> inferUnary expr [bool]
    EMul      {} -> ib (mulOp $ _eMOp expr)
    EAdd      {} -> ib [int, doub]
    ERel      {} -> inferBin (const bool) expr (relOp $ _eROp expr)
    EAnd      {} -> ib [bool]
    EOr       {} -> ib [bool]
    where ib = inferBin id expr

isPrimitive :: TypeA -> Bool
isPrimitive = \case
    Int  _ -> True
    Doub _ -> True
    Bool _ -> True
    _      -> False

arrayT :: TypeA -> Int -> TypeA
arrayT base dim = appConcrete make
    where make a = Array a base $ replicate dim $ DimenT emptyAnot

inferENew :: ExprA -> TCComp (ExprA, TypeA)
inferENew expr = do
    let typ  = _eTyp expr
    unless (isPrimitive typ) $ typeNotNewable typ
    let dims = _eDimEs expr
    dims' <- inferAccInts dims
    addTyp' (expr {  _eDimEs = dims' }) $ arrayT typ $ length dims

inferEVar :: Ident -> ExprA -> TCComp (ExprA, TypeA)
inferEVar name = lookupVarE' name >$> uncurry addTyp

inferEVar' :: Ident -> ExprA -> [DimEA] -> TCComp (ExprA, TypeA)
inferEVar' name expr dims = do
    (expr', typ) <- lookupVarE' name expr
    (bt, dimst)  <- inferArray (accOfNotArr name) typ
    let dimDiff = length dimst - length dims
    unless (dimDiff >= 0) $ accArrOverDimen name (length dimst) (length dims)
    let typ' = if dimDiff == 0 then bt else arrayT bt dimDiff
    (, typ') <$> (eDimEs %%~ inferAccInts $ expr')

inferArray :: (TypeA -> TCComp (TypeA, [DimTA]))
           ->  TypeA -> TCComp (TypeA, [DimTA])
inferArray onErr typ = maybe (onErr typ) extract $ typ ^? _Array
    where extract (_, bt, dimst) = return (bt, dimst)

inferAccInts :: [DimEA] -> TCComp [DimEA]
inferAccInts = mapM $ deExpr %%~ inferAccInt

inferAccInt :: ExprA -> TCComp ExprA
inferAccInt expr = do
    (expr', typ) <- inferExp expr
    unless (typ == int) (accArrNotInt typ) >> return expr'

inferLength :: ExprA -> TCComp (ExprA, TypeA)
inferLength = eExpr %%~ csub >$> (, int)
    where csub e = fst <$> inferExp e <<= inferArray lengthOfNotArr . snd

relOp :: RelOpA -> [TypeA]
relOp oper | oper `elem` (($ emptyAnot) <$> [NE, EQU]) = [int, doub, bool]
           | otherwise                                 = [int, doub]

mulOp :: MulOpA -> [TypeA]
mulOp oper | oper == Mod emptyAnot = [int]
           | otherwise             = [int, doub]

inferBin :: (TypeA -> TypeA) -> ExprA -> [TypeA] -> TCComp (ExprA, TypeA)
inferBin mTyp expr accept = do
    (exprl', typl) <- inferExp $ _eLExpr expr
    (exprr', typr) <- inferExp $ _eRExpr expr
    if typl == typr && typl `elem` accept
        then addTyp' (expr { _eLExpr = exprl', _eRExpr = exprr'}) (mTyp typl)
        else wrongBinExp (_eLExpr expr) (_eRExpr expr) typl typr

inferUnary :: ExprA -> [TypeA] -> TCComp (ExprA, TypeA)
inferUnary expr accept = do
    (expr', etyp) <- inferExp $ _eExpr expr
    if etyp `elem` accept
        then addTyp' (expr {_eExpr = expr'}) etyp
        else wrongUnaryExp expr accept etyp

inferFun :: ExprA -> TCComp (ExprA, TypeA)
inferFun expr = do
    FunSig texpected rtype <- lookupFunE $ _eIdent expr
    (exprs', tactual)      <- mapAndUnzipM inferExp $ _eAppExprs expr
    if texpected == tactual
        then addTyp' (expr { _eAppExprs = exprs' }) rtype
        else wrongArgsTyp (_eIdent expr) texpected tactual