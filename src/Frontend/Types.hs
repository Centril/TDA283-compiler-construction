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
Module      : Frontend.Types
Description : Types for Frontend of Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Types for Frontend of Javalette compiler.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Types (
    -- * Types
    TCEnv(..), Eval, EvalResult,
    Phase(..), ErrMsg(..), LogLevel, InfoLog, LogItem(..),
    Context, Contexts, Var(..),
    FunSig(..), FunId(..), FnSigMap,

    -- * Operations
    runEval, warn, warn', warnln, info, info', infoln, err, err', errln,
    initialEnv, pushBlock, popBlock,
    lookupVar', lookupFun', extendVar', extendFun',
    functions, contexts, toFunId, toFunSig
) where

import Safe

import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad()
import Control.Applicative()
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer

import Control.Lens hiding (Context, contexts)

import Utils.Pointless
import Utils.Foldable
import Utils.Monad

import Javalette.Abs

import Frontend.Annotations

--------------------------------------------------------------------------------
-- Scopes / Contexts:
--------------------------------------------------------------------------------

-- | 'Context': A context for a scope, map from variables -> types.
type Context = Map Ident TypeA

-- | 'Contexts': List of 'Context'
type Contexts = [Context]

-- | 'Var': a variable specified by its 'Ident' and 'Type'.
data Var = Var { vident :: Ident, vtype :: TypeA }
    deriving (Eq, Show, Read)

--------------------------------------------------------------------------------
-- Function Signatures:
--------------------------------------------------------------------------------

-- | 'FunSig': Signature of a function,
-- argument list (types) followed by return type.
data FunSig = FunSig { targs :: [TypeA], tret :: TypeA}
    deriving (Eq, Show, Read)

-- | 'FnId': Signature of a function ('FunSig') + 'Ident'.
data FunId = FunId { fident :: Ident, fsig :: FunSig}
    deriving (Eq, Show, Read)

-- | 'FnSigMap': Map of function identifiers -> signatures.
type FnSigMap = Map Ident FunSig

toFunSig :: ([ASTAnots -> TypeA], ASTAnots -> TypeA)
         -> FunSig
toFunSig (args, ret) = FunSig (applyEA args) (ret emptyAnot)

toFunId :: (String, ([ASTAnots -> TypeA], ASTAnots -> TypeA))
        -> FunId
toFunId (ident, sig) = FunId (Ident ident) $ toFunSig sig

--------------------------------------------------------------------------------
-- Operating Environment:
--------------------------------------------------------------------------------

-- | 'TCEnv': The operating environment of an 'Eval' computation.
data TCEnv = TCEnv {
    _functions :: FnSigMap,  -- ^ Map of ident -> function signatures.
    _contexts  :: Contexts } -- ^ Stack of contexts.
    deriving (Eq, Show, Read)

makeLenses ''TCEnv

-- | 'initialEnv': The initial empty environment.
initialEnv :: TCEnv
initialEnv = TCEnv Map.empty [Map.empty]

-- | 'pushBlock': pushes a fresh and empty block to the 'Context' stack.
pushBlock :: Eval ()
pushBlock = contexts %= (Map.empty:)

-- | 'popBlock': pops the top block from the 'Context' stack.
popBlock :: Eval ()
popBlock = contexts %= (fromMaybe [] . tailMay)

-- | 'lookupVar': If var exists in any scope in the 'Contexts', the 'Type' of
-- the identifier is '  return':ed, otherwise onErr is given the (var = 'Ident').
lookupVar' :: (Ident -> Eval TypeA) -> Ident -> Eval TypeA
lookupVar' onErr var = uses contexts (_lookupVar var) >>= maybeErr (onErr var)

_lookupVar :: Ident -> Contexts -> Maybe TypeA
_lookupVar = mfind . Map.lookup

-- | 'lookupFun': If function with given identifier exists, the 'FunSig' of it
-- is 'return':ed, otherwise, onErr is given the (var = 'Ident').
lookupFun' :: (Ident -> Eval FunSig) -> Ident -> Eval FunSig
lookupFun' onErr var = uses functions (Map.lookup var) >>= maybeErr (onErr var)

-- | 'extendVar': Extends the current scope with the given variable with ident
-- as 'Ident' and typ as 'Type'. If variable exists, onError is used.
extendVar' :: (Ident -> Eval ()) -> Var -> Eval ()
extendVar' onErr (Var ident typ) = do
    (c : ctxs) <- use contexts
    maybe (contexts .= Map.insert ident typ c:ctxs)
          (const $ onErr ident)
          (Map.lookup ident c)

-- | 'extendVar': Extends the accumulated function signatures with the given
-- signature as 'FnId'. If function exists, onError is used.
extendFun' :: (Ident -> Eval ()) -> FunId -> Eval ()
extendFun' onErr (FunId ident sig) = do
    funs <- use functions
    maybe (functions .= Map.insert ident sig funs)
          (const $ onErr ident)
          (Map.lookup ident funs)

--------------------------------------------------------------------------------
-- Computations in compiler:
--------------------------------------------------------------------------------

-- | 'Eval': A computation given typechecker state environment 'TCEnv',
-- potential fail-fast errors of type 'ErrMsg' and accumulated 'InfoLog's.
-- Monadic stack: StateT -> ExceptT -> WriterT -> Identity
-- See 'EvalResult' for details.
type Eval a = SEW TCEnv ErrMsg InfoLog a

-- | 'EvalResult': result of an 'Eval' computation.
type EvalResult a = (Either ErrMsg (a, TCEnv), InfoLog)

-- | 'runEval': Evaluates the entire 'Eval' computation given a 'TCEnv' to
-- work inside as starting environment.
runEval :: Eval a -> TCEnv -> EvalResult a
runEval = runSEW

-- SEW: SEWT using Identity monad.
type SEW s e w a = SEWT s e w Identity a

-- | SEWT: Composition of State . Except . Writer monad transformers in that
-- order where Writer is the innermost transformer.
-- the form of the computation is: s -> (Either e (a, s), w)
newtype SEWT s e w m a = SEWT {
    _runSEWT :: StateT s (ExceptT e (WriterT w m)) a }
    deriving (Functor, Applicative, Monad,
              MonadState s, MonadError e, MonadWriter w)

runSEW :: SEWT s e w Identity a -> s -> (Either e (a, s), w)
runSEW ev e = runIdentity $ runSEWT ev e

runSEWT :: SEWT s e w m a -> s -> m (Either e (a, s), w)
runSEWT ev e = runWriterT $ runExceptT $ runStateT (_runSEWT ev) e

--------------------------------------------------------------------------------
-- Errors, Logging:
--------------------------------------------------------------------------------

-- | 'Phase': Identifiers of various compilation phases.
data Phase = Parser | TypeChecker | ReturnChecker
    deriving (Eq, Show, Read, Ord, Enum)

-- | 'ErrMsg': Type of error messages in 'Err'.
-- They are annotated with a 'Phase' which denotes during
-- what compilation phase they occured.
data ErrMsg = ErrMsg { errPhase :: Phase, errMsg :: String }
    deriving (Eq, Show, Read)

-- | 'InfoLog': Type of the accumulated recoverable log messages.
type InfoLog = [LogItem]

-- | 'LogLevel': Denotes the level/severity of the logging. l_1 > l_2 in the
-- context of 'Ord' means that the serverity is higher / more severe.
data LogLevel = Info | Warn
    deriving (Eq, Show, Read, Enum, Ord)

-- | 'LogItem': Type of a recoverable error.
data LogItem = LogItem {
    logLvl :: LogLevel, logPhase :: Phase, logMsg :: String }
    deriving (Eq, Show, Read)

errln, err' :: Phase -> [String] -> Eval a
errln = unlines2nd err
err'  = unword2nd err

err :: Phase -> String -> Eval a
err = throwError .| ErrMsg

warn, info :: Phase -> String -> Eval ()
warn = _log Warn
info = _log Info

info', warn', infoln, warnln :: Phase -> [String] -> Eval ()
info'  = unword2nd  info
warn'  = unword2nd  warn
infoln = unlines2nd info
warnln = unlines2nd warn

_log :: LogLevel -> Phase -> String -> Eval ()
_log l p m = tell [LogItem l p m]

unword2nd, unlines2nd :: (t1 -> String -> t) -> t1 -> [String] -> t
unword2nd  f a b = f a $ unwords b
unlines2nd f a b = f a $ unlines b