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
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Types (
    -- * Types
    Env, FnSig, FnSigId, Sig, Err, Log,

    TCEnv (..), Eval, EvalResult,
    ErrMsg, InfoLog, LogItem(..),
    Context, Contexts,
    FunSig(..), FunId(..), FnSigMap,

    -- * Operations
    runEval, warn, warn', info, info', err,
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

import Utils.Foldable
import Utils.Monad

import Javalette.Abs

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
data FunId = FunId { fident :: Ident, fsig :: FunSig }

-- | 'FnSigMap': Map of function identifiers -> signatures.
type FnSigMap = Map Ident FunSig

toFunSig :: ([Type], Type) -> FunSig
toFunSig (args, ret) = FunSig args ret

toFunId :: (String, ([Type], Type)) -> FunId
toFunId (ident, sig) = FunId (Ident ident) $ toFunSig sig

--------------------------------------------------------------------------------
-- Operating Environment:
--------------------------------------------------------------------------------

-- | 'TCEnv': The operating environment of an 'Eval' computation.
data TCEnv = TCEnv {
    _functions :: FnSigMap, -- ^ Map of ident -> function signatures.
    _contexts  :: Contexts  -- ^ Stack of contexts.
}
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
lookupVar' :: (Ident -> Eval Type) -> Ident -> Eval Type
lookupVar' onErr var = uses contexts (_lookupVar var) >>= maybeErr (onErr var)

_lookupVar :: Ident -> Contexts -> Maybe Type
_lookupVar = mfind . Map.lookup

-- | 'lookupFun': If function with given identifier exists, the 'FunSig' of it
-- is 'return':ed, otherwise, onErr is given the (var = 'Ident').
lookupFun' :: (Ident -> Eval FunSig) -> Ident -> Eval FunSig
lookupFun' onErr var = uses functions (Map.lookup var) >>= maybeErr (onErr var)

-- | 'extendVar': Extends the current scope with the given variable with ident
-- as 'Ident' and typ as 'Type'. If variable exists, onError is used.
extendVar' :: (Ident -> Eval ()) -> Ident -> Type -> Eval ()
extendVar' onErr ident typ = do
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

-- | 'ErrMsg': Type of error messages in 'Err'.
type ErrMsg = String

-- | 'InfoLog': Type of the accumulated recoverable log messages.
type InfoLog = [LogItem]

-- | 'LogItem': Type of a recoverable error.
data LogItem = Warn { _warn :: String } | Info { _info :: String }

err :: String -> Eval a
err = throwError

_log :: MonadWriter [t] m => t -> m ()
_log w = tell [w]

warn, info :: String -> Eval ()
warn = _log . Warn
info = _log . Info

info', warn' :: [String] -> Eval ()
warn' = warn . unwords
info' = info . unwords

--------------------------------------------------------------------------------
-- OLD: @TODO REMOVE
--------------------------------------------------------------------------------
type Err =     Either ErrMsg
type Log =     String
type FnSig =   ([Type], Type)
type FnSigId = (Ident, FnSig)
type Sig =     Map Ident FnSig
type Env =     (Sig, [Context])