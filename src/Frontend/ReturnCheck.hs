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

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Monoid

import Control.Monad
import Control.Arrow

import Control.Lens

import Utils.Pointless
import Utils.Monad

import Javalette.Abs

import Frontend.Types
import Frontend.Query
import Frontend.Error

-- temporary:
import Frontend.Example
import Utils.Debug

deriving instance Enum RelOp

u = undefined

data Literal = LBool { lbool :: Bool } | LInt { lint :: Integer } |
               LDouble { ldouble :: Double } | LString { lstring :: String }
    deriving Show

makePrisms ''Literal

returnCheck :: Env -> Program -> Err Env
returnCheck env = foldM returnCheckFun env . progFuns

returnCheckFun :: Env -> TopDef -> Err Env
returnCheckFun env (FnDef rtype ident _ (Block block))
    | rtype == Void = Right env
    | otherwise     = case any checkHasReturn block of
                      True  -> Right env
                      False -> Left $ noFunRet ident

checkHasReturn :: Stmt -> Bool
checkHasReturn stmt = case stmt of
    Ret _               -> True
    VRet                -> True
    BStmt (Block subs)  -> any checkHasReturn subs
    Cond expr st        -> case evalCondExpr expr of
        Just True       -> checkHasReturn st
        _               -> False
    CondElse expr s1 s2 -> case evalCondExpr expr of
        Just True       -> checkHasReturn s1
        Just False      -> checkHasReturn s2
        Nothing         -> all checkHasReturn [s1, s2]
    While expr st       -> case evalCondExpr expr of
        Just True       -> checkHasReturn st
        _               -> False
    _                   -> False

evalCondExpr :: Expr -> Maybe Bool
evalCondExpr expr = evalConstExpr expr >>= (^? _LBool)

evalConstExpr :: Expr -> Maybe Literal
evalConstExpr expr = case expr of
    ELitTrue   -> pure $ LBool True
    ELitFalse  -> pure $ LBool False
    ELitInt  v -> pure $ LInt v
    ELitDoub v -> pure $ LDouble v
    EString  v -> pure $ LString v
    EVar _     -> Nothing
    EApp _ _   -> Nothing
    EOr  l r   -> evalBoolOp (||) l r
    EAnd l r   -> evalBoolOp (&&) l r
    Not  e     -> LBool . not <$> detLitBool e
    Neg  e     -> evalNeg e
    EMul l o r -> evalBinOp l r
                    (\v er -> mulFn (Just mod) div o <!> v <*>
                              mulFetchRight o er _LInt)
                    (\v er -> mulFn Nothing (/) o <!> v <*>
                              mulFetchRight o er _LDouble)
    EAdd l o r -> evalBinOp l r
                    (\v er -> plusFn o <:> v <*> er ^? _LInt)
                    (\v er -> plusFn o <:> v <*> er ^? _LDouble)
    ERel l o r ->  do
        el <- evalConstExpr l
        er <- evalConstExpr r
        fmap LBool $ case el of
            LBool v   -> relFn <$> mfilter (`elem` [EQU .. NE]) (pure o)
                         <*> pure v <*> er ^? _LBool
            LInt v    -> evalRelStd v o er _LInt
            LDouble v -> evalRelStd v o er _LDouble
            LString v -> evalRelStd v o er _LString

evalBoolOp :: (Bool -> Bool -> Bool) -> Expr -> Expr -> Maybe Literal
evalBoolOp op l r = liftM2 (LBool .| op) (detLitBool l) (detLitBool r)

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
mulFn m d Times = Just (*)
mulFn m d Div   = Just d
mulFn m d Mod   = m