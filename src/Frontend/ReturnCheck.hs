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

import Control.Lens hiding (op, re)

import Utils.Pointless
import Utils.Monad
import Utils.Function

import Javalette.Abs

import Frontend.Annotations
import Frontend.Computation
import Frontend.Error

returnCheck :: ProgramA -> Eval ProgramA
returnCheck (Program a funs) = Program a <$> mapM checkFunRet funs

checkFunRet :: TopDefA -> Eval TopDefA
checkFunRet fun@(FnDef a rtype ident args block)
    | rtype == tvoid = return fun
    | otherwise      = FnDef a rtype ident args <<$> checkBlockTop ident block

checkBlockTop :: Ident -> BlockA -> Eval (BlockA, Bool)
checkBlockTop fid block = do
    r@(_, hasRet) <- checkBlock fid block
    if hasRet then return r else insufficientFunRet fid

checkBlock :: Ident -> BlockA -> Eval (BlockA, Bool)
checkBlock fid (Block a stmts) =
    (Block a *** or) <$> mapAndUnzipM (checkHasRet fid) stmts

checkBStmt :: ASTAnots -> BlockA -> Ident -> Eval (StmtA, Bool)
checkBStmt a block fid = first (addWE Always . BStmt a) <$> checkBlock fid block

checkHasRet :: Ident -> StmtA -> Eval (StmtA, Bool)
checkHasRet fid stmt = case stmt of
    While    a expr st    -> checkCond While a expr st    fid
    Cond     a expr st    -> checkCond Cond  a expr st    fid
    CondElse a expr si se -> checkCondElse   a expr si se fid
    BStmt    a block      -> checkBStmt      a block      fid
    Ret  {}               -> addWE' stmt Always True
    VRet {}               -> addWE' stmt Always True
    _                     -> addWE' stmt Always False

checkCond :: (ASTAnots -> ExprA -> StmtA -> StmtA)
          ->  ASTAnots -> ExprA -> StmtA -> Ident
          -> Eval (StmtA, Bool)
checkCond ctor a expr stmt fid = first (addWE Always) <$> case we of
    Always -> checkRetWrap fid stmt' $ ctor a expr'
    _      -> return (ctor a expr' stmt', False)
    where (expr', we) = condExpr expr
          stmt'       = addWE we stmt

checkCondElse :: ASTAnots -> ExprA -> StmtA -> StmtA -> Ident
              -> Eval (StmtA, Bool)
checkCondElse a expr si se fid = first (addWE Always) <$> case we of
    Always  -> checkRetWrap fid si' $ flip (CondElse a expr') se'
    Never   -> checkRetWrap fid se' $ CondElse a expr' si'
    Unknown -> do (si'', siRet) <- checkHasRet fid si'
                  (se'', seRet) <- checkHasRet fid se'
                  return (CondElse a expr' si'' se'', siRet && seRet)
    where (expr', we) = condExpr expr
          (si', se')  = addWE § (we, weOpposite we) <§> (si, se)

weOpposite :: WillExecute -> WillExecute
weOpposite Always  = Never
weOpposite Never   = Always
weOpposite Unknown = Unknown

condExpr :: ExprA -> (ExprA, WillExecute)
condExpr = second (toWillExecute . (>>= (^? _LBool))) . evalConstExpr

checkRetWrap :: Ident -> StmtA -> (StmtA -> StmtA) -> Eval (StmtA, Bool)
checkRetWrap fid stmt ctor = first ctor <$> checkHasRet fid stmt

evalConstExpr :: ExprA -> (ExprA, ML)
evalConstExpr expr = case expr of
    EVar {}        -> addLit  expr Nothing
    EApp {}        -> addLit  expr Nothing
    ELitTrue  _    -> addLit' expr $ LBool True
    ELitFalse _    -> addLit' expr $ LBool False
    ELitInt   _ v  -> addLit' expr $ LInt    v
    ELitDoub  _ v  -> addLit' expr $ LDouble v
    EString   _ v  -> addLit' expr $ LString v
    Not  a e       -> evalNot a e
    Neg  a e       -> evalNeg a e
    EOr  a l r     -> evalBoolOp (||) EOr  a l r
    EAnd a l r     -> evalBoolOp (&&) EAnd a l r
    EMul a l op r  -> evalMul a l op r
    EAdd a l op r  -> evalAdd a l op r
    ERel a l op r  -> evalRel a l op r

evalNot :: ASTAnots -> ExprA -> (ExprA, ML)
evalNot a expr = addLit (Not a expr') $ LBool . not <$> lit
    where (expr', lit) = detLitBool expr

evalNeg :: ASTAnots -> ExprA -> (ExprA, ML)
evalNeg a expr = addLit (Neg a expr') $ lit >>= f
    where (expr', lit)  = evalConstExpr expr
          f (LInt    v) = neg LInt    v
          f (LDouble v) = neg LDouble v
          f _           = Nothing

evalBoolOp :: (Bool -> Bool -> Bool)
           -> (ASTAnots -> ExprA -> ExprA -> ExprA)
           ->  ASTAnots -> ExprA -> ExprA
           -> (ExprA, Maybe Literal)
evalBoolOp op ctor a l r = addLit (ctor a l' r') $ liftM2 (LBool .| op) ll rl
    where ((l', ll), (r', rl)) = (detLitBool l, detLitBool r)

detLitBool :: ExprA -> (ExprA, Maybe Bool)
detLitBool expr = second (>>= (^? _LBool)) $ evalConstExpr expr

evalBin ::  ExprA -> ExprA
        -> (ExprA -> ExprA -> Maybe Literal -> (ExprA, Maybe Literal))
        -> (Maybe Literal  -> Maybe Literal -> Maybe Literal)
        -> (ExprA, Maybe Literal)
evalBin l r g f = g l' r' $ f llit rlit
    where ((l', llit), (r', rlit)) = (evalConstExpr l, evalConstExpr r)

evalAdd :: ASTAnots -> ExprA -> AddOpA -> ExprA -> (ExprA, ML)
evalAdd a l op r = evalArith EAdd a l op r (handle _LInt) (handle _LDouble)
    where handle c v er = plusFn op <:> v <*> er ^? c

evalMul :: ASTAnots -> ExprA -> MulOpA -> ExprA -> (ExprA, ML)
evalMul a l o r = evalArith EMul a l o r hint hdoub
    where hint  v er = mulFn (Just mod) div o <!> v <*> mulFetchRight o er _LInt
          hdoub v er = mulFn Nothing (/) o <!> v <*> mulFetchRight o er _LDouble

evalArith :: (ASTAnots -> ExprA -> t1 -> ExprA -> ExprA)
          ->  ASTAnots -> ExprA -> t1 -> ExprA
          -> (Integer -> Literal -> Maybe Integer)
          -> (Double  -> Literal -> Maybe Double)
          -> (ExprA, Maybe Literal)
evalArith ctor a l op r hint hdoub = evalBin l r wrap makeLit
    where wrap l' r' = addLit $ ctor a l' op r'
          makeLit llit rlit = do
            (ll, rl) <- liftM2 (,) llit rlit
            case ll of LInt    v -> LInt    <$> hint v rl
                       LDouble v -> LDouble <$> hdoub v rl
                       _         -> Nothing

evalRel :: ASTAnots -> ExprA -> RelOpA -> ExprA -> (ExprA, ML)
evalRel a l op r = evalBin l r (\l' r' -> addLit $ ERel a l' op r') makeLit
    where makeLit llit rlit = do
            (ll, rl) <- liftM2 (,) llit rlit
            fmap LBool $ case ll of
                LBool   v -> relFn <$> mfilter isBoolRel (pure op)
                             <*> pure v <*> rl ^? _LBool
                LInt    v -> evalRelStd v op rl _LInt
                LDouble v -> evalRelStd v op rl _LDouble
                LString v -> evalRelStd v op rl _LString

neg :: (Monad m, Num v) => (v -> r) -> v -> m r
neg ctor v = return $ ctor $ -v

evalRelStd :: Ord a => a -> RelOpA -> s -> Getting (First a) s a -> Maybe Bool
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

mulFetchRight :: (Eq a, Num a) => MulOpA -> Literal ->
                 Getting (First a) Literal a -> Maybe a
mulFetchRight o er p = mfilter (\r -> r /= 0 || void o /= Div ()) $ er ^? p

isBoolRel :: RelOpA -> Bool
isBoolRel = (`elem` [EQU (), NE ()]) . void