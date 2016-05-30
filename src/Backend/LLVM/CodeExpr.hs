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
Module      : Backend.LLVM.CodeExpr
Description : Codegen for ExprA in LLVM backend of the Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Codegen for ExprA in LLVM backend of the Javalette compiler.
-}
{-# LANGUAGE LambdaCase, TupleSections #-}

module Backend.LLVM.CodeExpr where

import Data.List
import Data.Tuple
import Data.Maybe
import qualified Data.Map as M

import Control.Monad

import Control.Lens hiding (Empty, op, uncons, to, pre, ix)

import Utils.Monad
import Utils.Sizeables

import Common.AST
import Common.Annotations

import Backend.LLVM.Environment
import Backend.LLVM.Types
import Backend.LLVM.CodeDSL
import Backend.LLVM.CodeComplex

import Frontend.Environment as F

compileCondExpr :: ExprA -> LLabelRef -> LLabelRef -> LComp ()
compileCondExpr c _then _else = compileExpr c >>= condBr _then _else

compileExpr :: ExprA -> LComp LTValRef
compileExpr = \case
    ENew      a _ ds    -> compileENew a ds
    EVar      a lval    -> compileEVar a lval
    ELitInt   _ v       -> return $ intTVR  v
    ELitDoub  _ v       -> return $ doubTVR v
    EString   _ v       -> compileCString   v
    ELitTrue  _         -> return ltrue
    ELitFalse _         -> return lfalse
    ECastNull _ t       -> compileCastNull t
    EApp      a i es    -> compileApp a i es
    EMApp     a lv m es -> compileMApp a lv m es
    Incr      _ lval    -> compileInDeCr lval Plus  False
    Decr      _ lval    -> compileInDeCr lval Minus False
    PreIncr   _ lval    -> compileInDeCr lval Plus  True
    PreDecr   _ lval    -> compileInDeCr lval Minus True
    Neg       _ e       -> compileNeg e
    Not       _ e       -> compileNot e
    EMul      _ l o r   -> compileMul  o l r
    EAdd      _ l o r   -> compileAdd  o l r
    ERel      _ l o r   -> compileLRel o l r
    EAnd      _ l   r   -> compileLBin l r 0 "land"
    EOr       _ l   r   -> compileLBin l r 1 "lor"

compileENew :: ASTAnots -> [DimEA] -> LComp LTValRef
compileENew = compileNew . getType

compileNew :: TypeA -> [DimEA] -> LComp LTValRef
compileNew t@(TStruct {}) [] = compileType t >>= \lbt -> salloc lbt return lbt
compileNew t@(TRef    {}) [] = compileType t >>= \lbt -> salloc lbt return lbt
compileNew typ          (d:ds) = do
    (bt, lbt) <- fkeep compileType $ shrink typ
    l         <- compileExpr $ _deExpr d
    (compileType typ >>= salloc (LPtr lbt) (imul l >=> iadd lenSize)) <<= setLength l
        <<= \newed -> unless (null ds) $ basicFor "newSubArr" lbt newed l $
            (compileNew bt ds >>=) . flip store

salloc :: LType -> (LTValRef -> LComp LTValRef) -> LType -> LComp LTValRef
salloc et sc rt = compileSizeof et >>= sc >>= flip compileCalloc ione
                                          >>= bitcast rt

compileEVar :: ASTAnots -> LValueA -> LComp LTValRef
compileEVar anots lval = do
    ltyp <- compileType $ getType anots
    compileLVal lval >>= load ltyp

compileCString :: String -> LComp LTValRef
compileCString v = do
    ref  <- newConstRef "cstring"
    let typ   = LArray (1 + genericLength v) charType
    pushConst $ LConstGlobal ref typ v
    assignTemp strType $ strPointer (LPtr typ) ref

compileCastNull :: TypeA -> LComp LTValRef
compileCastNull = compileType >$> flip LTValRef LNull

compileApp :: ASTAnots -> Ident -> [ExprA] -> LComp LTValRef
compileApp anots name es = do
    fr <- LFunRef False (_ident name) <$> mapM compileExpr es
    compileAnotType anots >>= callFSwitch fr

callFSwitch :: LFunRef -> LType -> LComp LTValRef
callFSwitch fr = \case
        LVoid -> vcall fr
        rtyp  -> assignCall rtyp fr

compileMApp :: ASTAnots -> LValueA -> Ident -> [ExprA] -> LComp LTValRef
compileMApp anots lv mname es = do
    let typ  = extractType lv
    cls     <- getClass $ _tIdent typ
    let meth = getMethod mname cls
    let virt = extractVirt $ fst $ snd meth
    les     <- mapM compileExpr es
    this    <- compileLVal lv
    ttyp    <- compileType typ
    tload   <- load ttyp this
    let dispatch = if virt then compileCallDynamic else compileCallStatic
    dispatch cls meth this les

hasVirtual :: [F.ClassInfo] -> Bool
hasVirtual cls = or $ extractVirt <$> (cls >>= M.elems . F._ciMethods)

nameMethod :: F.ClassInfo -> FnDefA -> LIdent
nameMethod cl fn = _ident (F._ciIdent cl) ++ "__" ++ _ident (_fIdent fn)

compileCallStatic :: [F.ClassInfo] -> (Integer, (FnDefA, F.ClassInfo))
                  -> LTValRef -> LTValRefs
                  -> LComp LTValRef
compileCallStatic _ (_, (fn, cl)) this les = do
    let name = nameMethod cl fn
    let fr   = LFunRef False name (this : les)
    lrtyp   <- compileType $ _fRetTyp fn
    callFSwitch fr lrtyp

nameVTable :: String -> String
nameVTable name = name ++ "__vtable"

-- TODO: Check if this is correct
vtableOf :: LType -> LType
vtableOf (LAlias alias) = LAlias $ nameVTable alias
vtableOf (LPtr ptr)     = vtableOf ptr
vtableOf _              = error "vtableOf no alias found"

compileCallDynamic :: [F.ClassInfo] -> (Integer, (FnDefA, F.ClassInfo))
                  -> LTValRef -> LTValRefs
                  -> LComp LTValRef
compileCallDynamic _ (ix, (fn, cl)) this les = do
    let ltyp  = _lTType this
    -- load vtable:
    let vttyp = vtableOf ltyp
    vtabref  <- assignPtr vttyp $ deref [izero] this
    vtable   <- load vttyp vtabref
    -- load method:
    ftyp     <- compileType $ toFnPtr fn
    methref  <- assignPtr ftyp  $ deref [intTVR ix] vtable
    methload <- load ftyp methref
    -- call method:
    let fr   = LFunRef True (_lRIdent $ _lTVRef methload) (this : les)
    lrtyp   <- compileType $ _fRetTyp fn
    callFSwitch fr lrtyp

toFnPtr :: FnDefA -> TypeA
toFnPtr fn = let FunSig args ret = F.toFnSig fn in Fun emptyAnot ret args

compileInDeCr :: LValueA -> (ASTAnots -> AddOpA) -> Bool -> LComp LTValRef
compileInDeCr lval op isPre = do
    let typ = extractType lval
    mem  <- compileLVal lval
    ltyp <- compileType typ
    pre  <- load ltyp mem
    post <- assignTemp ltyp $ switchAdd (op emptyAnot) ltyp pre $
                              switchType (LVInt 1) (LVFloat 1) ltyp
    store post mem
    return $ if isPre then pre else post

compileNeg :: ExprA -> LComp LTValRef
compileNeg e = do
    LTValRef t e' <- compileExpr e
    assignTemp t $ switchType (LSub izero) (LFSub dzero) t e'

compileNot :: ExprA -> LComp LTValRef
compileNot = compileExpr >=> assignBool . flip LXor (LVInt 1)

compileMul :: MulOpA -> ExprA -> ExprA -> LComp LTValRef
compileMul op = compileBArith id $
                switchType (compileIMulOp op) (compileFMulOp op)

compileIMulOp :: MulOpA -> LTValRef -> LValRef -> LExpr
compileIMulOp = \case
    Times _ -> LMul
    Div   _ -> LDiv
    -- C++ (ISO 2011), https://en.wikipedia.org/wiki/Modulo_operation
    Mod   _ -> LSRem

compileFMulOp :: MulOpA -> LTValRef -> LValRef -> LExpr
compileFMulOp = \case
    Times _ -> LFMul
    Div   _ -> LFDiv
    Mod   _ -> LFRem -- will never happen, but added for completeness.

compileAdd :: AddOpA -> ExprA -> ExprA -> LComp LTValRef
compileAdd = compileBArith id . switchAdd

switchAdd :: AddOpA -> LType -> LTValRef -> LValRef -> LExpr
switchAdd op = switchType (compileIAddOp op) (compileFAddOp op)

compileIAddOp :: AddOpA -> LTValRef -> LValRef -> LExpr
compileIAddOp = \case
    Plus  _ -> LAdd
    Minus _ -> LSub

compileFAddOp :: AddOpA -> LTValRef -> LValRef -> LExpr
compileFAddOp = \case
    Plus  _ -> LFAdd
    Minus _ -> LFSub

switchType :: t -> t -> LType -> t
switchType onI onF = \case
    LInt   _  -> onI
    LFloat _  -> onF
    x         -> error $ "switchType got wrong type, " ++ show x ++ "."

compileBArith :: (LType -> LType) -> (LType -> LTValRef -> LValRef -> LExpr)
               -> ExprA -> ExprA -> LComp LTValRef
compileBArith th gf l r = do
    l'            <- compileExpr l
    LTValRef t r' <- compileExpr r
    assignTemp (th t) (gf t l' r')



compileLRel :: RelOpA -> ExprA -> ExprA -> LComp LTValRef
compileLRel op l r = do
    ll <- compileExpr l
    let (c, f) = case _lTType ll of
                 LPtr   _ -> (LICmp . compileRelOpI, ptrToInt)
                 LInt   _ -> (LICmp . compileRelOpI, return)
                 LFloat _ -> (LFCmp . compileRelOpF, return)
                 x        -> error $ "compileLRel got wrong type, " ++ show x
    ll' <- f ll
    compileExpr r >>= f >>= assignBool . c op ll' . _lTVRef

compileRelOpI :: RelOpA -> LICmpOp
compileRelOpI = \case
    LTH _ -> LSlt
    LE  _ -> LSle
    GTH _ -> LSgt
    GE  _ -> LSge
    EQU _ -> LEq
    NE  _ -> LNe

compileRelOpF :: RelOpA -> LFCmpOp
compileRelOpF = \case
    LTH _ -> LFOlt
    LE  _ -> LFOle
    GTH _ -> LFOgt
    GE  _ -> LFOge
    EQU _ -> LFOeq
    NE  _ -> LFOne

compileLBin :: ExprA -> ExprA -> Integer -> String -> LComp LTValRef
compileLBin l r onLHS prefix = do
    [lRhs, lEnd] <- newLabels prefix ["rhs", "end"]
    uncurry (compileCondExpr l) $
        (if onLHS == 0 then id else swap) (lRhs, lEnd)
    lLhs          <- lastLabel
    LTValRef _ r' <- xInLabel lRhs lEnd $ compileExpr r
    lRhs'         <- lastLabel
    compileLabel lEnd
    assignBool $ LPhi boolType [LPhiRef (LVInt onLHS) lLhs, LPhiRef r' lRhs']

--------------------------------------------------------------------------------
-- LValues:
--------------------------------------------------------------------------------

compileLVal :: LValueA -> LComp LTValRef
compileLVal = \case
    LValueS a lvl lvr    -> do
        let lvlT = extractType lvl
        ltyp <- compileType lvlT
        llvl <- compileLVal lvl >>= load ltyp
        case lvlT of
            Array   {}      -> lengthRef llvl
            TStruct _ sname -> accStruct llvl a lvr sname
            TRef    _ cname -> accClass llvl a lvr cname
            _               -> error "compileLVal: should not pass type check!"
    LValueV a name dimes ->
        arrAccs a dimes $ return . flip ptrRef (_ident $ markIfArg a name)

accStruct :: LTValRef -> ASTAnots -> LValueA -> Ident -> LComp LTValRef
accStruct llvl a lvr sname =
    let LValueV _ rname dimes = lvr -- Works since LValue tree is left assoc.
    in arrAccs a dimes $ \ltopT -> intTVR <$> lookupFieldIx rname sname >>=
                                   assignPtr ltopT . flip deref llvl . return

-- TODO: Rewrite because it happens for self.
accClass :: LTValRef -> ASTAnots -> LValueA -> Ident -> LComp LTValRef
accClass llvl a lvr cname = u

lookupFieldIx :: Ident -> Ident -> LComp Integer
lookupFieldIx rname = getStructDef >$>
                      _sfIndex . fromJust . find ((rname ==) . _sfIdent)

markIfArg :: ASTAnots -> Ident -> Ident
markIfArg anots = ident %~ (case mayVS anots of Just VSArg -> "p"; _ -> ""; ++)

arrAccs :: ASTAnots -> [DimEA] -> (LType -> LComp LTValRef) -> LComp LTValRef
arrAccs a dimes topf = do
    let ts@(topt:bts) = reverse $ take (1 + length dimes) (growInf $ getType a)
    top <- compileType topt >>= topf
    foldM arrAcc top $ zip3 dimes bts ts

arrAcc :: LTValRef -> (DimEA, TypeA, TypeA) -> LComp LTValRef
arrAcc top (dime, bT, topT) = do
    idx <- compileExpr $ _deExpr dime
    lbT <- compileType bT
    compileType topT >>= flip load top >>= assignPtr lbT . deref [ione, idx]