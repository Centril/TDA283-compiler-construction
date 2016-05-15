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
    EVar _ n []  -> inferEVar n expr
    EVar _ n ds  -> inferEVar' n expr ds
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

-- TODO: Rename and implement the functions in Error.hs
u = undefined
typeNotNewable = u
arrAccNotInt = u
accOfNotArr = u
accOfNotArrNoName = u
arrDimsNotMatching = u

isPrimitive :: TypeA -> Bool
isPrimitive = \case
    Int  _ -> True
    Doub _ -> True
    Bool _ -> True
    _      -> False

inferENew :: ExprA -> TCComp (ExprA, TypeA)
inferENew expr = do
    let typ  = _eTyp expr
    unless (isPrimitive typ) $ typeNotNewable typ expr
    let dims = _eDimEs expr
    dims' <- mapM (deExpr %%~ inferInt) dims
    let typ' = appConcrete $ \a ->
                Array a typ $ replicate (length dims) (DimenT emptyAnot)
    addTyp' (expr {  _eDimEs = dims' }) typ'

inferEVar :: Ident -> ExprA -> TCComp (ExprA, TypeA)
inferEVar name = lookupVarE' name >$> uncurry addTyp

inferEVar' :: Ident -> ExprA -> [DimEA] -> TCComp (ExprA, TypeA)
inferEVar' name expr dims = do
    (expr', typ) <- lookupVarE' name expr
    dimst <- inferArray (accOfNotArr name) typ
    unless (length dimst == length dims) $ arrDimsNotMatching name
    dims' <- mapM (deExpr %%~ inferInt) dims
    return $ (expr' { _eDimEs = dims' }, typ)

inferArray :: (TypeA -> TCComp [DimTA]) -> TypeA -> TCComp [DimTA]
inferArray onErr typ = do
    case typ ^? _Array of
        Nothing -> onErr typ
        Just (_, _, dimst) -> return dimst

inferInt :: ExprA -> TCComp ExprA
inferInt expr = do
    (expr', typ) <- inferExp expr
    if typ /= int
        then arrAccNotInt expr
        else return expr'

inferLength :: ExprA -> TCComp (ExprA, TypeA)
inferLength expr = do
    expr' <- eExpr %%~ inferLengthH $ expr
    return (expr', int)

inferLengthH :: ExprA -> TCComp ExprA
inferLengthH e = do
    (e', typ) <- inferExp e
    inferArray (accOfNotArrNoName) typ
    return e'

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