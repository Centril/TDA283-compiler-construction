{- Javalette Compiler
   Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -}

{-|
Module      : Frontend.TypeCheck
Description : Type checker for Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
Stability   : experimental
Portability : ALL

Type checker for Javalette compiler.
-}
module Frontend.TypeCheck2 (
    -- * Types
    Env, Sig, Context, Err, Log,

    -- * Operations
    typeCheck
) where

import qualified Data.Map as Map

import Control.Monad
import Control.Arrow

import Utils.Foldable

import Javalette.Abs

import Frontend.Types
import Frontend.Query
import Frontend.ReturnCheck
import Frontend.Error2

-- temporary:
import Utils.Debug
u = undefined

typeCheck :: Program -> Eval Program
typeCheck prog = u

mainId :: Ident
mainId = Ident "main"

mainCorrect :: Eval ()
mainCorrect = lookupFun' mainNotFound mainId >>=
              flip unless wrongMainSig . (== FunSig [] Int)

allFunctions :: Program -> Eval ()
allFunctions = u

collectFuns :: [FunId] -> Eval ()
collectFuns = u

predefFuns :: [FunId]
predefFuns = map toFunId
    [("printInt",    ([Int],      Void)),
     ("printDouble", ([Doub],     Void)),
     ("printString", ([ConstStr], Void)),
     ("readInt",     ([],         Int)),
     ("readDouble",  ([],         Doub))]

extractFunIds :: Program -> [FunId]
extractFunIds = map toFnSigId' . progFuns


progFunSigs :: Program -> [FunId]
progFunSigs = map toFnSigId . progFuns

toFnSigId :: TopDef -> FunId
toFnSigId = u

checkProg :: Program -> Eval Program
checkProg = u

inferExp :: Expr -> Err (Expr, Type)
inferExp = u

relOp :: RelOp -> [Type]
relOp oper | oper `elem` [NE, EQU] = [Int, Doub, Bool]
           | otherwise             = [Int, Doub]

mulOp :: MulOp -> [Type]
mulOp oper | oper == Mod = [Int]
           | otherwise   = [Int, Doub]

inferBinary :: [Type] -> Expr -> Expr -> Eval (Expr, Expr, Type)
inferBinary types exp1 exp2 = u

inferUnary :: [Type] -> Expr -> Eval (Expr, Type)
inferUnary types expr = u

inferFun :: Ident -> [Expr] -> Eval Type
inferFun ident exprs = u

checkFun :: TopDef -> Eval TopDef
checkFun (FnDef rtype _ args block) = u

checkBlock :: Type -> Block -> Eval Block
checkBlock typ (Block block) = u

checkStms :: Type -> [Stmt] -> Eval [Stmt]
checkStms typ = u

checkStm :: Type -> Stmt -> Eval Stmt
checkStm typ stmt = u

checkExp :: Type -> Expr -> Eval (Expr, Type)
checkExp ty1 expr = u

checkIdeExp :: Ident -> Expr -> Eval (Expr, Type)
checkIdeExp ident expr = u

checkIdent :: [Type] -> Ident -> Eval Type
checkIdent types ident = u

checkVoid :: Type -> Eval Type
checkVoid typ = u

checkDecl :: Type -> Item -> Eval Item
checkDecl typ item = u