{- Javalette Compiler
   Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -}

{-|
Module      : Frontend.Types
Description : Types for Frontend of Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
Stability   : experimental
Portability : ALL

Types for Frontend of Javalette compiler.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving,FlexibleContexts #-}

module Frontend.Types (
    -- * Types
    Env, FnSig, FnSigId, Sig, Err, Log,

    TCEnv, Eval, EvalResult,
    ErrMsg, InfoLog, LogItem,
    Context, Contexts,
    FunSig, FnId, FnSigMap,

    -- * Operations
    runEval, warn, warn', info, info',
) where

import Data.Map

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer

import Javalette.Abs

--------------------------------------------------------------------------------
-- OLD: @TODO REMOVE
--------------------------------------------------------------------------------
type Err =     Either ErrMsg
type Log =     String
type FnSig =   ([Type], Type)
type FnSigId = (Ident, FnSig)
type Sig =     Map Ident FnSig
type Env =     (Sig, [Context])

--------------------------------------------------------------------------------
-- Computational & Operating Environment:
--------------------------------------------------------------------------------

-- | 'TCEnv': The operating environment of an 'Eval' computation.
data TCEnv = TCEnv {
    functions :: FnSigMap, -- ^ Map of ident -> function signatures.
    contexts  :: Contexts  -- ^ Stack of contexts.
}

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

-- SEWT: State + Except + Writer monad with inner
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

-- | 'ErrMsg': Type of error messages in 'Err'.
type ErrMsg = String

-- | 'InfoLog': Type of the accumulated recoverable log messages.
type InfoLog = [LogItem]

-- | 'LogItem': Type of a recoverable error.
data LogItem = Warn { _warn :: String } | Info { _info :: String }

_log :: MonadWriter [t] m => t -> m ()
_log w = tell [w]

warn, info :: String -> Eval ()
warn = _log . Warn
info = _log . Info

info', warn' :: [String] -> Eval ()
warn' = warn . unwords
info' = info . unwords

--------------------------------------------------------------------------------
-- Scopes / Contexts:
--------------------------------------------------------------------------------

-- | 'Context': A context for a scope, map from variables -> types.
type Context = Map Ident Type

-- | 'Contexts': List of 'Context'
type Contexts = [Context]

--------------------------------------------------------------------------------
-- Function Signatures:
--------------------------------------------------------------------------------

-- | 'FunSig': Signature of a function,
-- argument list (types) followed by return type.
data FunSig = FunSig { targs :: [Type], tret :: Type }

-- | 'FnId': Signature of a function ('FunSig') + 'Ident'.
data FnId = FnId { fident :: Ident, fsig :: FnSig }

-- | 'FnSigMap': Map of function identifiers -> signatures.
type FnSigMap = Map Ident FnSig