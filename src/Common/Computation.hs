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
Module      : Common.Computation
Description : Computation monad and operations in Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Computation monad and operations in Javalette compiler.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common.Computation (
    -- * Types
    Comp, CompResult,
    Phase(..), ErrMsg(..), LogLevel, InfoLog, LogItem(..),

    -- * Operations
    runComp, warn, warn', warnln, info, info', infoln, err, err', errln
) where

import Control.Monad()
import Control.Applicative()
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer

import Utils.Pointless

--------------------------------------------------------------------------------
-- Computations in compiler:
--------------------------------------------------------------------------------

-- | 'Comp': A computation given an environment or state s,
-- potential fail-fast errors of type 'ErrMsg' and accumulated 'InfoLog's.
-- Monadic stack: StateT -> ExceptT -> WriterT -> Identity
-- See 'CompResult' for details.
type Comp s a = SEW s ErrMsg InfoLog a

-- | 'CompResult': result of a 'Comp' computation.
type CompResult s a = (Either ErrMsg (a, s), InfoLog)

-- | 'runComp': Evaluates the entire 'Comp' computation given a state to work
-- inside as starting environment / state.
runComp :: Comp s a -> s -> CompResult s a
runComp = runSEW

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
data Phase = Parser | TypeChecker | ReturnChecker |
             AlphaRenamer | Compiler | Linker
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

errln, err' :: Phase -> [String] -> Comp s a
errln = unlines2nd err
err'  = unword2nd err

err :: Phase -> String -> Comp s a
err = throwError .| ErrMsg

warn, info :: Phase -> String -> Comp s ()
warn = _log Warn
info = _log Info

info', warn', infoln, warnln :: Phase -> [String] -> Comp s ()
info'  = unword2nd  info
warn'  = unword2nd  warn
infoln = unlines2nd info
warnln = unlines2nd warn

_log :: LogLevel -> Phase -> String -> Comp s ()
_log l p m = tell [LogItem l p m]

unword2nd, unlines2nd :: (t1 -> String -> t) -> t1 -> [String] -> t
unword2nd  f a b = f a $ unwords b
unlines2nd f a b = f a $ unlines b