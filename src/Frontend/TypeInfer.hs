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
{-# LANGUAGE TupleSections, LambdaCase,
             TypeSynonymInstances, FlexibleInstances #-}

module Frontend.TypeInfer (
    -- * Operations
    inferExp, inferType, inferLVal
) where

import Control.Monad

import Control.Lens
import Control.Lens.Extras

import Utils.Monad
import Utils.Sizeables

import Common.AST

import Frontend.Environment
import Frontend.Error
import Frontend.Common

u = undefined

--------------------------------------------------------------------------------
-- Kind inference:
--------------------------------------------------------------------------------

-- TODO: For now, all types have a concrete kind, change this?
inferType :: TypeA -> TCComp (TypeA, Kind)
inferType typ = do
    typ1 <- tTyp %%~ (inferType >$> fst) $ typ
    return $ addKind typ1 KConcrete

--------------------------------------------------------------------------------
-- Type inference:
--------------------------------------------------------------------------------

inferExp :: ExprA -> TCComp (ExprA, TypeA)
inferExp expr = case expr of
    ENew      {} -> inferENew expr
    EVar      {} -> inferEVar expr
    EString   {} -> addTyp' expr conststr
    ELitInt   {} -> addTyp' expr int
    ELitDoub  {} -> addTyp' expr doub
    ELitTrue  _  -> addTyp' expr bool
    ELitFalse _  -> addTyp' expr bool
    ECastNull {} -> inferCastNull expr
    EApp      {} -> inferFun expr
    Incr      {} -> inferIncrDecr expr
    Decr      {} -> inferIncrDecr expr
    PreIncr   {} -> inferIncrDecr expr
    PreDecr   {} -> inferIncrDecr expr
    Neg       {} -> inferUnary expr [int, doub]
    Not       {} -> inferUnary expr [bool]
    EMul      {} -> ib (mulOp $ _eMOp expr)
    EAdd      {} -> ib [int, doub]
    ERel      {} -> inferBin (const bool) expr (relOp $ _eROp expr)
    EAnd      {} -> ib [bool]
    EOr       {} -> ib [bool]
    where ib = inferBin id expr

inferCastNull :: ExprA -> TCComp (ExprA, TypeA)
inferCastNull expr = do
    (typ, _) <- inferType $ _eTyp expr
    unless (isPointable typ) $ nullNotCastable typ
    addTyp' expr { _eTyp = typ } typ

inferENew :: ExprA -> TCComp (ExprA, TypeA)
inferENew expr = do
    (bt, _)  <- inferType $ _eTyp expr
    unless (notArray bt) $ typeNotNewable bt
    dims     <- inferAccInts $ _eDimEs expr
    (typ, _) <- inferType $ arrayT (bt, length dims)
    addTyp' (expr { _eDimEs = dims, _eTyp = bt }) typ

inferEVar :: ExprA -> TCComp (ExprA, TypeA)
inferEVar expr = do
    (lval, typ) <- inferLVal $ _eLVal expr
    addTyp' expr { _eLVal = lval } typ

checkEVar :: [TypeA] -> ExprA -> TCComp (ExprA, TypeA)
checkEVar allowed expr = do
    r@(_, typ) <- inferEVar expr
    unless (typ `elem` allowed) (wrongExprTyp expr allowed typ)
    return r

inferIncrDecr :: ExprA -> TCComp (ExprA, TypeA)
inferIncrDecr = checkEVar [int, doub]

inferLVal :: LValueA -> TCComp (LValueA, TypeA)
inferLVal lval = case lval of
    LValueS _ lvl lvr    -> do
        (lvl', typ) <- inferLVal lvl
        if is _Array typ then do
            unless (isLVLength lvr) (arrayNotStruct typ)
            addTyp' lval { _lvLLVal = lvl' } int
        else if is _TStruct typ then
            -- TODO: ensure typ is a struct
            -- TODO: ensure lvr exists in struct.
            return u
        else primNoAccProps typ
    LValueV _ name []    -> uncurry addTyp <$> lookupVarE' name lval
    LValueV _ name dimes -> do
        (lval', typ) <- lookupVarE' name lval
        (bt, dimst)  <- inferArray (accOfNotArr name) typ
        let (ldts, ldes) = (length dimst, length dimes)
        let dimDiff = ldts - ldes
        unless (dimDiff >= 0) $ accArrOverDimen name ldts ldes
        flip addTyp (growN dimDiff bt) <$> (lvDimEs %%~ inferAccInts $ lval')

isLVLength :: LValueA -> Bool
isLVLength = \case
    LValueV _ (Ident "length") [] -> True
    _                             -> False

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