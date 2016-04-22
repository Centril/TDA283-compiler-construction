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
Module      : Frontend.ReturnCheck
Description : Return checker for Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Return checker for Javalette compiler,
part of type checker that makes sure all branches
that need do return.
-}
module Frontend.ReturnCheck (
    -- * Operations
    returnCheck
) where

import Data.Monoid

import Control.Arrow
import Control.Monad

import Control.Lens

import Utils.Pointless
import Utils.Monad

import Javalette.Abs

import Frontend.Annotations
import Frontend.Computation
import Frontend.Error

returnCheck :: ProgramA -> Eval ProgramA
returnCheck (Program a funs) = Program a <$> mapM checkFunRet funs

checkFunRet :: TopDefA -> Eval TopDefA
checkFunRet fun@(FnDef a rtype ident args block)
    | void rtype == Void () = return fun
    | otherwise     = FnDef a rtype ident args <<$> checkBlockTop ident block

checkBlockTop :: Ident -> BlockA -> Eval (BlockA, Bool)
checkBlockTop fid block = do
    r@(_, hasRet) <- checkBlock fid block
    if hasRet then return r else insufficientFunRet fid

checkBlock :: Ident -> BlockA -> Eval (BlockA, Bool)
checkBlock fid (Block a stmts) =
    (Block a *** or) <$> mapAndUnzipM (checkHasRet fid) stmts

checkHasRet :: Ident -> StmtA -> Eval (StmtA, Bool)
checkHasRet fid stmt = case stmt of
    Ret _ _               -> return (stmt, True)
    VRet _                -> return (stmt, True)
    BStmt a block         -> first (BStmt a) <$> checkBlock fid block
    While a expr st       -> checkCond (While a) fid expr st
    Cond  a expr st       -> checkCond (Cond a) fid expr st
    CondElse a expr si se -> checkCondElse a fid expr si se
    _                     -> return (stmt, False)

checkCond :: (ExprA -> StmtA -> StmtA) -> Ident
          ->  ExprA -> StmtA -> Eval (StmtA, Bool)
checkCond ctor fid expr stmt =
    case condLit mlit of
    Always -> checkRetWrap fid stmt $ ctor expr
    _      -> return (ctor expr stmt, False)
    where mlit = evalConstExpr expr

checkCondElse :: ASTAnots -> Ident -> ExprA -> StmtA -> StmtA
              -> Eval (StmtA, Bool)
checkCondElse a fid expr si se =
    case condLit mlit of
    Always  -> checkRetWrap fid si $ flip (CondElse a expr) se
    Never   -> checkRetWrap fid se $ CondElse a expr si
    Unknown -> do
        (si', siRet) <- checkHasRet fid si
        (se', seRet) <- checkHasRet fid se
        return (CondElse a expr si' se', siRet && seRet)
    where mlit = evalConstExpr expr

checkRetWrap :: Ident -> StmtA -> (StmtA -> StmtA) -> Eval (StmtA, Bool)
checkRetWrap fid stmt ctor = first ctor <$> checkHasRet fid stmt

condLit :: Maybe Literal -> WillExecute
condLit mlit = toWillExecute $ mlit >>= (^? _LBool)

evalConstExpr :: ExprA -> Maybe Literal
evalConstExpr expr = case expr of
    EVar {}      -> Nothing
    EApp {}      -> Nothing
    ELitTrue _   -> pure $ LBool True
    ELitFalse _  -> pure $ LBool False
    ELitInt _  v -> pure $ LInt v
    ELitDoub _ v -> pure $ LDouble v
    EString _  v -> pure $ LString v
    Not _  e     -> LBool . not <$> detLitBool e
    Neg _  e     -> evalNeg e
    EOr _  l r   -> evalBoolOp (||) l r
    EAnd _ l r   -> evalBoolOp (&&) l r
    EMul _ l o r -> evalMul l o r
    EAdd _ l o r -> evalAdd l o r
    ERel _ l o r -> evalRel l o r

evalMul :: ExprA -> MulOpA -> ExprA -> Maybe Literal
evalMul l o r = evalBinOp l r int doub
    where int  v er = mulFn (Just mod) div o <!> v <*> mulFetchRight o er _LInt
          doub v er = mulFn Nothing (/) o <!> v <*> mulFetchRight o er _LDouble

evalAdd :: ExprA -> AddOpA -> ExprA -> Maybe Literal
evalAdd l o r = evalBinOp l r int doub
    where int  v er = plusFn o <:> v <*> er ^? _LInt
          doub v er = plusFn o <:> v <*> er ^? _LDouble

evalRel :: ExprA -> RelOpA -> ExprA -> Maybe Literal
evalRel l o r = do
    el <- evalConstExpr l
    er <- evalConstExpr r
    fmap LBool $ case el of
        LBool   v -> relFn <$> mfilter isBoolRel (pure o) <*>
                     pure v <*> er ^? _LBool
        LInt    v -> evalRelStd v o er _LInt
        LDouble v -> evalRelStd v o er _LDouble
        LString v -> evalRelStd v o er _LString

isBoolRel :: RelOpA -> Bool
isBoolRel = (`elem` [EQU (), NE ()]) . void

evalBoolOp :: (Bool -> Bool -> Bool) -> ExprA -> ExprA -> Maybe Literal
evalBoolOp o le ri = liftM2 (LBool .| o) (detLitBool le) (detLitBool ri)

detLitBool :: ExprA -> Maybe Bool
detLitBool x = evalConstExpr x >>= (^? _LBool)

mulFetchRight :: (Eq a, Num a) => MulOpA -> Literal ->
                 Getting (First a) Literal a -> Maybe a
mulFetchRight o er p = mfilter (\r -> r /= 0 || void o /= Div ()) $ er ^? p

evalBinOp :: ExprA -> ExprA -> (Integer -> Literal -> Maybe Integer)
          -> (Double -> Literal -> Maybe Double) -> Maybe Literal
evalBinOp l r manipI manipD = do
        el <- evalConstExpr l
        er <- evalConstExpr r
        case el of LInt v    -> LInt <$> manipI v er
                   LDouble v -> LDouble <$> manipD v er
                   _         -> Nothing

evalNeg :: ExprA -> Maybe Literal
evalNeg e = evalConstExpr e >>= f
    where f (LInt v)    = neg LInt v
          f (LDouble v) = neg LDouble v
          f _           = Nothing

neg :: (Monad m, Num v) => (v -> r) -> v -> m r
neg ctor v = return $ ctor $ -v

evalRelStd :: Ord a => a -> RelOpA -> s
           -> Getting (First a) s a -> Maybe Bool
evalRelStd v o er t = relFn o <:> v <*> er ^? t

relFn :: Ord a => RelOpA -> a -> a -> Bool
relFn (LTH _) = (<)
relFn (LE  _) = (<=)
relFn (GTH _) = (>)
relFn (GE  _) = (>=)
relFn (EQU _) = (==)
relFn (NE  _) = (/=)

plusFn :: Num a => AddOpA -> a -> a -> a
plusFn (Plus  _) = (+)
plusFn (Minus _) = (-)

mulFn :: Num a
      => Maybe (a -> a -> a) -> (a -> a -> a)
      -> MulOpA -> Maybe (a -> a -> a)
mulFn _ _ (Times _) = Just (*)
mulFn _ d (Div   _) = Just d
mulFn m _ (Mod   _) = m