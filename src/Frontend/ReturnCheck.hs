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

import Control.Monad
import Control.Arrow

import Control.Lens

import Utils.Pointless

import Javalette.Abs

import Frontend.Types
import Frontend.Query

-- temporary:
import Frontend.Example
import Utils.Debug

deriving instance Enum RelOp

u = undefined

(<!>) :: Applicative f => f (a -> b) -> a -> f b
(<!>) f = (<*>) f . pure
infixr 7 <!>

(<:>) :: Applicative f => (a -> b) -> a -> f b
(<:>) f = fmap f . pure
infixr 8 <:>

data Literal = LBool { lbool :: Bool } | LInt { lint :: Integer } |
               LDouble { ldouble :: Double } | LString { lstring :: String }
    deriving Show
makePrisms ''Literal

returnCheck :: Program -> Err Env
returnCheck prog = u

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
                    (\v er -> mulFn (Just mod) div o <!> v <*> er ^? _LInt)
                    (\v er -> mulFn Nothing (/) o <!> v <*> er ^? _LDouble)
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

evalBinOp :: Expr -> Expr -> (Integer -> Literal -> Maybe Integer)
          -> (Double -> Literal -> Maybe Double) -> Maybe Literal
evalBinOp l r manipI manipD = do
        el <- evalConstExpr l
        er <- evalConstExpr r
        case el of LInt v    -> LInt <$> manipI v er
                   LDouble v -> LDouble <$> manipD v er
                   _         -> Nothing

evalNeg e = evalConstExpr e >>= f
    where f (LInt v)    = neg LInt v
          f (LDouble v) = neg LDouble v
          f _           = Nothing

neg :: (Monad m, Num v) => (v -> r) -> v -> m r
neg ctor v = return $ ctor $ -v

evalRelStd v o er t = relFn o <:> v <*> er ^? t

relFn LTH = (<)
relFn LE  = (<=)
relFn GTH = (>=)
relFn GE  = (>)
relFn EQU = (==)
relFn NE  = (/=)

plusFn Plus  = (+)
plusFn Minus = (-)

mulFn m d Times = Just (*)
mulFn m d Div   = Just d
mulFn m d Mod   = m


evalBoolOp :: (Bool -> Bool -> Bool) -> Expr -> Expr -> Maybe Literal
evalBoolOp op l r = liftM2 (LBool .| op) (detLitBool l) (detLitBool r)

detLitBool :: Expr -> Maybe Bool
detLitBool x = evalConstExpr x >>= (^? _LBool)