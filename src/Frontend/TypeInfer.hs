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
    inferExp, inferType, inferLVal, checkExp
) where

import Data.List
import qualified Utils.GraphFlip as GF
import qualified Data.Map        as M

import Control.Arrow
import Control.Monad

import Control.Lens

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

checkExp :: TypeA -> ExprA -> TCComp ExprA
checkExp texpected expr = do
    (expr', tactual) <- inferExp expr
    assignableTo tactual texpected
        >>= flip unless (wrongExpTyp expr texpected tactual)
    return expr'

inferExp :: ExprA -> TCComp (ExprA, TypeA)
inferExp expr = case expr of
    ENew      {} -> inferENew expr
    EVar      {} -> inferEVar expr
    EString   {} -> lit conststr
    ELitInt   {} -> lit int
    ELitDoub  {} -> lit doub
    ELitTrue  _  -> lit bool
    ELitFalse _  -> lit bool
    ECastNull {} -> inferCastNull expr
    EApp      {} -> inferFun expr
    EMApp     {} -> inferMeth expr
    Incr      {} -> iid
    Decr      {} -> iid
    PreIncr   {} -> iid
    PreDecr   {} -> iid
    Neg       {} -> iu [int, doub]
    Not       {} -> iu [bool]
    EMul      {} -> ib (mulOp $ _eMOp expr)
    EAdd      {} -> ib [int, doub]
    ERel      {} -> inferBin (const bool) expr (relOp $ _eROp expr)
    EAnd      {} -> ib [bool]
    EOr       {} -> ib [bool]
    where ib accept = inferBin id expr (`elem` accept)
          iid = inferIncrDecr expr
          lit = addTyp' expr
          iu  = inferUnary expr

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
    (typ, _) <- inferType $ growN (length dims) bt
    addTyp' (expr { _eDimEs = dims, _eTyp = bt }) typ

inferEVar :: ExprA -> TCComp (ExprA, TypeA)
inferEVar expr = first (flip (eLVal .~) expr) <$> inferLVal (_eLVal expr)
                 >>= uncurry addTyp'

checkEVar :: [TypeA] -> ExprA -> TCComp (ExprA, TypeA)
checkEVar allowed expr = do
    r@(_, typ) <- inferEVar expr
    unlessAssignableAny typ allowed $ wrongExprTyp expr allowed typ
    return r

inferIncrDecr :: ExprA -> TCComp (ExprA, TypeA)
inferIncrDecr = checkEVar [int, doub]

inferLVal :: LValueA -> TCComp (LValueA, TypeA)
inferLVal lval = case lval of
    LValueS _ lvl lvr -> do
        (lvl', ltyp) <- inferLVal lvl
        case ltyp of
            Array   {}      -> do
                unless (isLVLength lvr) (arrayNotStruct ltyp)
                addTyp' lval { _lvLLVal = lvl' } int
            TStruct _ sname -> do
                -- Works because the LValue tree is left associative:
                let LValueV _ rname dimes = lvr
                typ <- _sfType <$> lookupField sname rname
                (lvr', rtyp) <- case dimes of
                    [] -> addTyp' lvr typ
                    _  -> inferLValArr lvr typ
                addTyp' lval { _lvLLVal = lvl', _lvRLVal = lvr' } rtyp
            TRef _ cname    -> do
                ic <- use inClass
                unless ic noAccPrivProp
                -- Works because the LValue tree is left associative:
                let LValueV _ rname dimes = lvr
                typ <- _sfType <$> lookupProp cname rname
                (lvr', rtyp) <- case dimes of
                    [] -> addTyp' lvr typ
                    _  -> inferLValArr lvr typ
                addTyp' lval { _lvLLVal = lvl', _lvRLVal = lvr' } rtyp
            _         -> primNoAccProps ltyp
    LValueV _ name [] -> adjustPropInMeth $
                         uncurry addTyp <$> lookupVarE' name lval
    LValueV _ name _  -> adjustPropInMeth $
                         lookupVarE' name lval >>= uncurry inferLValArr

adjustPropInMeth :: TCComp (LValueA, t) -> TCComp (LValueA, t)
adjustPropInMeth mlv = do
    (lv, typ) <- mlv
    lv' <- case extractVS lv of
           VSThis     -> return lv {_lvIdent = Ident "this" }
           VSProp cid -> do
            ctyp <- lookupTypeName (error "never happens") cid
            let this0 = LValueV emptyAnot (Ident "this") []
            let (this1, _) = addTyp (addSource this0 VSThis) ctyp
            return $ LValueS (_lvAnot lv) this1 lv
           _          -> return lv
    return (lv', typ)

inferLValArr :: LValueA -> TypeA -> TCComp (LValueA, TypeA)
inferLValArr lv typ = do
    let (name, dimes) = _lvIdent &&& _lvDimEs $ lv
    (bt, dimst) <- inferArray (accOfNotArr name) typ
    let (ldts, ldes) = (length dimst, length dimes)
    let dimDiff = ldts - ldes
    unless (dimDiff >= 0) $ accArrOverDimen name ldts ldes
    flip addTyp (growN dimDiff bt) <$> (lvDimEs %%~ inferAccInts $ lv)

lookupProp :: Ident -> Ident -> TCComp SFieldA
lookupProp cname rname = do
    fields <- _ciFields <$> lookupClass' noSuchClass cname
    typ    <- lookupTypeName noSuchTypeName cname
    maybeErr (propNotExists rname typ) $ find ((rname ==) . _sfIdent) fields

lookupField :: Ident -> Ident -> TCComp SFieldA
lookupField sname rname = do
    fields <- lookupStruct noSuchTypeName sname
    typ    <- lookupTypeName noSuchTypeName sname
    maybeErr (propNotExists rname typ) $ find ((rname ==) . _sfIdent) fields

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
    unlessAssignable typ int (accArrNotInt typ) >> return expr'

relOp :: RelOpA -> TypeA -> Bool
relOp oper | oper `elem` (($ emptyAnot) <$> [NE, EQU]) = const True
           | otherwise                                 = (`elem` [int, doub])

mulOp :: MulOpA -> [TypeA]
mulOp oper | oper == Mod emptyAnot = [int]
           | otherwise             = [int, doub]

inferBin :: (TypeA -> TypeA) -> ExprA -> (TypeA -> Bool) -> TCComp (ExprA, TypeA)
inferBin mTyp expr acceptf = do
    (exprl', typl) <- inferExp $ _eLExpr expr
    (exprr', typr) <- inferExp $ _eRExpr expr
    refl1 <- assignableTo typl typr
    refl2 <- assignableTo typr typl
    unless ((refl1 || refl2) && acceptf typl) $
        wrongBinExp (_eLExpr expr) (_eRExpr expr) typl typr
    addTyp' (expr { _eLExpr = exprl', _eRExpr = exprr'}) (mTyp typl)

inferUnary :: ExprA -> [TypeA] -> TCComp (ExprA, TypeA)
inferUnary expr accept = do
    (expr', etyp) <- inferExp $ _eExpr expr
    unlessAssignableAny etyp accept $ wrongUnaryExp expr accept etyp
    addTyp' (expr {_eExpr = expr'}) etyp

inferFun :: ExprA -> TCComp (ExprA, TypeA)
inferFun expr = do
    sig <- lookupFunE $ _eIdent expr
    inferFunX expr sig

inferFunX :: ExprA -> FunSig -> TCComp (ExprA, TypeA)
inferFunX expr (FunSig texpected rtype) = do
    (exprs', tactual)      <- mapAndUnzipM inferExp $ _eAppExprs expr
    unlessAssignableAll tactual texpected $
        wrongArgsTyp (_eIdent expr) texpected tactual
    addTyp' (expr { _eAppExprs = exprs' }) rtype

inferMeth :: ExprA -> TCComp (ExprA, TypeA)
inferMeth expr = do
    (lv, lvtyp) <- inferLVal $ _eLVal expr
    clName      <- maybeErr (methAppOnNotClass lvtyp) $ snd <$> lvtyp ^? _TRef
    cls         <- lookupClass noSuchClass clName
    meth        <- lookupMethod classDoesntHaveMethod (_eIdent expr) cls
    inferFunX expr { _eLVal = lv } $ toFnSig meth

--------------------------------------------------------------------------------
-- Type assignability rules:
--------------------------------------------------------------------------------

unlessAssignableAny :: TypeA -> [TypeA] -> TCComp () -> TCComp ()
unlessAssignableAny tfrom tto onNot = or <$> mapM (assignableTo tfrom) tto
                                      >>= flip unless onNot

unlessAssignableAll :: [TypeA] -> [TypeA] -> TCComp () -> TCComp ()
unlessAssignableAll tfrom tto onNot = do
    unless (length tfrom == length tto) onNot
    and <$> zipWithM assignableTo tfrom tto >>= flip unless onNot

unlessAssignable :: TypeA -> TypeA -> TCComp () -> TCComp ()
unlessAssignable tfrom tto onNot = assignableTo tfrom tto >>= flip unless onNot

assignableTo :: TypeA -> TypeA -> TCComp Bool
assignableTo tfrom tto = case tfrom of
    Array    _ bt ds -> (&& ds == _tDimTs tto) <$> assignableTo bt (_tTyp tto)
    TRef     _ i     -> assignableClass i (_tIdent tto)
    _                -> return $ tfrom == tto

assignableClass :: Ident -> Ident -> TCComp Bool
assignableClass ifrom_ ito_ = do
    (conv, gr) <- uses classGraph $ first (M.!)
    return $ GF.connected gr (conv ito_) (conv ifrom_)