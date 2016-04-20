{- Javalette Compiler
   Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -}

{-|
Module      : Frontend.ReturnCheck
Description : Return checker for Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
Stability   : experimental
Portability : ALL

Return checker for Javalette compiler,
part of type checker that makes sure all branches
that need do return.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

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

import Frontend.Types
import Frontend.Query
import Frontend.Error

deriving instance Enum RelOp

data Literal = LBool Bool | LInt Integer | LDouble Double | LString String
    deriving (Eq, Show, Read, Ord)

makePrisms ''Literal

data WillExecute = Always | Never | Unknown
    deriving (Eq, Show, Read, Ord, Enum)

toWillExecute :: Maybe Bool -> WillExecute
toWillExecute (Just True)  = Always
toWillExecute (Just False) = Never
toWillExecute Nothing      = Unknown

returnCheck :: Program -> Eval Program
returnCheck = fmap Program . mapM checkFun . progFuns

checkFun :: TopDef -> Eval TopDef
checkFun fun@(FnDef rtype ident args block)
    | rtype == Void = return fun
    | otherwise     = FnDef rtype ident args <<$> checkBlockTop ident block

checkBlockTop :: Ident -> Block -> Eval (Block, Bool)
checkBlockTop fid block = do
    r@(_, hasRet) <- checkBlock fid block
    if hasRet then return r else insufficientFunRet fid

checkBlock :: Ident -> Block -> Eval (Block, Bool)
checkBlock fid (Block stmts) =
    (Block *** or) <$> mapAndUnzipM (checkHasRet fid) stmts

checkHasRet :: Ident -> Stmt -> Eval (Stmt, Bool)
checkHasRet fid stmt = case stmt of
    Ret _               -> return (stmt, True)
    VRet                -> return (stmt, True)
    BStmt block         -> first BStmt <$> checkBlock fid block
    While expr st       -> checkCond While fid expr st
    Cond  expr st       -> checkCond Cond  fid expr st
    CondElse expr si se -> checkCondElse fid expr si se
    _                   -> return (stmt, False)

checkCond :: (Expr -> Stmt -> Stmt) -> Ident
          ->  Expr -> Stmt ->          Eval (Stmt, Bool)
checkCond ctor fid expr stmt =
    case condLit mlit of
    Always -> checkRetWrap fid stmt $ ctor expr
    _      -> return (ctor expr stmt, False)
    where mlit = evalConstExpr expr

checkCondElse :: Ident -> Expr -> Stmt -> Stmt -> Eval (Stmt, Bool)
checkCondElse fid expr si se =
    case condLit mlit of
    Always  -> checkRetWrap fid si $ flip (CondElse expr) se
    Never   -> checkRetWrap fid se $ CondElse expr si
    Unknown -> do
        (si', siRet) <- checkHasRet fid si
        (se', seRet) <- checkHasRet fid se
        return (CondElse expr si' se', siRet && seRet)
    where mlit = evalConstExpr expr

checkRetWrap :: Ident -> Stmt -> (Stmt -> Stmt) -> Eval (Stmt, Bool)
checkRetWrap fid stmt ctor = first ctor <$> checkHasRet fid stmt

condLit :: Maybe Literal -> WillExecute
condLit mlit = toWillExecute $ mlit >>= (^? _LBool)

evalConstExpr :: Expr -> Maybe Literal
evalConstExpr expr = case expr of
    EVar _     -> Nothing
    EApp _ _   -> Nothing
    ELitTrue   -> pure $ LBool True
    ELitFalse  -> pure $ LBool False
    ELitInt  v -> pure $ LInt v
    ELitDoub v -> pure $ LDouble v
    EString  v -> pure $ LString v
    Not  e     -> LBool . not <$> detLitBool e
    Neg  e     -> evalNeg e
    EOr  l r   -> evalBoolOp (||) l r
    EAnd l r   -> evalBoolOp (&&) l r
    EMul l o r -> evalMul l o r
    EAdd l o r -> evalAdd l o r
    ERel l o r -> evalRel l o r

evalMul :: Expr -> MulOp -> Expr -> Maybe Literal
evalMul l o r = evalBinOp l r int doub
    where int  v er = mulFn (Just mod) div o <!> v <*> mulFetchRight o er _LInt
          doub v er = mulFn Nothing (/) o <!> v <*> mulFetchRight o er _LDouble

evalAdd :: Expr -> AddOp -> Expr -> Maybe Literal
evalAdd l o r = evalBinOp l r int doub
    where int  v er = plusFn o <:> v <*> er ^? _LInt
          doub v er = plusFn o <:> v <*> er ^? _LDouble

evalRel :: Expr -> RelOp -> Expr -> Maybe Literal
evalRel l o r = do
    el <- evalConstExpr l
    er <- evalConstExpr r
    fmap LBool $ case el of
        LBool   v -> relFn <$> mfilter (`elem` [EQU .. NE]) (pure o) <*>
                     pure v <*> er ^? _LBool
        LInt    v -> evalRelStd v o er _LInt
        LDouble v -> evalRelStd v o er _LDouble
        LString v -> evalRelStd v o er _LString

evalBoolOp :: (Bool -> Bool -> Bool) -> Expr -> Expr -> Maybe Literal
evalBoolOp o le ri = liftM2 (LBool .| o) (detLitBool le) (detLitBool ri)

detLitBool :: Expr -> Maybe Bool
detLitBool x = evalConstExpr x >>= (^? _LBool)

mulFetchRight :: (Eq a, Num a) => MulOp -> Literal ->
                 Getting (First a) Literal a -> Maybe a
mulFetchRight o er p = mfilter (\r -> r /= 0 || o /= Div) $ er ^? p

evalBinOp :: Expr -> Expr -> (Integer -> Literal -> Maybe Integer)
          -> (Double -> Literal -> Maybe Double) -> Maybe Literal
evalBinOp l r manipI manipD = do
        el <- evalConstExpr l
        er <- evalConstExpr r
        case el of LInt v    -> LInt <$> manipI v er
                   LDouble v -> LDouble <$> manipD v er
                   _         -> Nothing

evalNeg :: Expr -> Maybe Literal
evalNeg e = evalConstExpr e >>= f
    where f (LInt v)    = neg LInt v
          f (LDouble v) = neg LDouble v
          f _           = Nothing

neg :: (Monad m, Num v) => (v -> r) -> v -> m r
neg ctor v = return $ ctor $ -v

evalRelStd :: Ord a => a -> RelOp -> s -> Getting (First a) s a -> Maybe Bool
evalRelStd v o er t = relFn o <:> v <*> er ^? t

relFn :: Ord a => RelOp -> a -> a -> Bool
relFn LTH = (<)
relFn LE  = (<=)
relFn GTH = (>)
relFn GE  = (>=)
relFn EQU = (==)
relFn NE  = (/=)

plusFn :: Num a => AddOp -> a -> a -> a
plusFn Plus  = (+)
plusFn Minus = (-)

mulFn :: Num a
      => Maybe (a -> a -> a) -> (a -> a -> a)
      -> MulOp -> Maybe (a -> a -> a)
mulFn _ _ Times = Just (*)
mulFn _ d Div   = Just d
mulFn m _ Mod   = m