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

{-# LANGUAGE FlexibleContexts #-}

module Common.Computation (
    -- * Modules
    module Common.Options,

    -- * Types
    Comp, CompResult, IOComp, IOCompResult,
    Phase(..), ErrMsg(..), LogLevel, InfoLog, LogItem(..),

    -- * Operations

    -- ** Monad stack transformations
    rebase, io, transST, changeST, ($:<<),

    -- ** Running computations
    runComp, runIOComp,

    -- ** Logging and Error operations
    warn, warn', warnln, info, info', infoln, infoP, err, err', errln,
    showLogItem, showLogs, printLogs, showError, printError
) where

import Control.Arrow
import Control.Monad()
import Control.Applicative()
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Morph

import Control.Lens (view)

import Utils.Pointless
import Utils.Terminal

import Common.Options

--------------------------------------------------------------------------------
-- Computations:
--------------------------------------------------------------------------------

-- | 'Comp': A computation given an environment or state s,
-- potential fail-fast errors of type 'ErrMsg' and accumulated 'InfoLog's.
-- See 'CompResult' for details.
type Comp s a = RSEW s JlcOptions ErrMsg InfoLog a

-- | 'CompResult': result of a 'Comp' computation.
type CompResult s a = (Either ErrMsg (a, s), InfoLog)

-- | 'runComp': Evaluates the entire 'Comp' computation given a read only state,
-- and a state to work inside as starting environment.
runComp :: Comp s a -> JlcOptions -> s -> CompResult s a
runComp = runRSEW

--------------------------------------------------------------------------------
-- IO Computations:
--------------------------------------------------------------------------------

-- | 'IOComp': like 'Comp' but with 'IO' instead of 'Identity' as base monad.
type IOComp s a = RSEWT s JlcOptions ErrMsg InfoLog IO a

-- | 'IOCompResult': result of an 'IOComp' computation.
type IOCompResult s a = IO (CompResult s a)

-- | 'runIOComp': Evaluates the entire 'IOComp' computation given a state to
-- work inside as starting environment / state.
runIOComp :: IOComp s a -> JlcOptions -> s -> IOCompResult s a
runIOComp = runRSEWT

-- | 'io': alias of 'liftIO'
io :: MonadIO m => IO a -> m a
io = liftIO

--------------------------------------------------------------------------------
-- RSEWT:
--------------------------------------------------------------------------------

-- RSEW: RSEWT using Identity monad.
type RSEW s r e w a = RSEWT s r e w Identity a

-- | RSEWT: Composition of ReaderT . StateT . ExceptT . WriterT
-- monad transformers in that order where Writer is the innermost transformer.
-- the form of the computation is: r -> s -> (Either e (a, s), w)
newtype RSEWT s r e w m a = RSEWT {
    _runRSEWT :: ReaderT r (StateT s (ExceptT e (WriterT w m))) a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO,
              MonadReader r, MonadState s, MonadError e, MonadWriter w)

-- 'runRSEW': runs a 'RSEW' computation given initial states,
-- specialization of 'runRSEWT' for the 'Identity' base monad.
runRSEW :: RSEWT s r e w Identity a -> r -> s -> (Either e (a, s), w)
runRSEW r s m = runIdentity $ runRSEWT r s m

-- | 'runRSEWT': runs a 'RSEWT' computation given initial states.
runRSEWT :: RSEWT s r e w m a -> r -> s -> m (Either e (a, s), w)
runRSEWT ev r s = runWriterT $ runExceptT $
                runStateT (runReaderT (_runRSEWT ev) r) s

instance Monoid w => MonadTrans (RSEWT s r e w) where
    lift = RSEWT . lift . lift . lift . lift

instance Monoid w => MFunctor (RSEWT s r e w) where
    hoist f = RSEWT . hoist (hoist (hoist (hoist f))) . _runRSEWT

-- | 'transST': transitions between one 'RSEWT' computation with state x, to
-- another with state y. The current state and result of the given computation
-- is given to a mapping function that must produce the next computation.
-- The initial state must also be passed as the last parameter.
transST :: Functor m
         => (((a, y), x) -> (a, y))
         -> RSEWT x r e w m a -> x -> RSEWT y r e w m a
transST f x_c x_i =
    RSEWT $ ReaderT $ \r -> StateT $ \y_i -> ExceptT . WriterT $
    first ((\(a, x_f) -> f ((a, y_i), x_f)) <$>) <$> runRSEWT x_c r x_i

-- | '$:<<': infix version 'transST'
($:<<) :: Functor m
        => (((a, y), x) -> (a, y))
        -> RSEWT x r e w m a -> x -> RSEWT y r e w m a
($:<<)  = transST

-- 'changeST': specialization of 'transST', ignoring final state of the
-- first computation directly using the initial of the second computation.
changeST :: Functor m => RSEWT x r e w m a -> x -> RSEWT y r e w m a
changeST =  transST fst

-- | 'rebase': change base monad of from Identity to something else.
rebase :: (MFunctor t, Monad n) => t Identity b -> t n b
rebase = hoist generalize

--------------------------------------------------------------------------------
-- Errors, Logging:
--------------------------------------------------------------------------------

-- | 'Phase': Identifiers of various compilation phases.
data Phase = Parser | TypeChecker | ReturnChecker |
             AlphaRenamer | PreOptimizer |
             Compiler | Linker
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

-- | 'err': throws an error in a computation ignoring any continuations.
err :: Phase -> String -> Comp s a
err = throwError .| ErrMsg

warn, info :: Phase -> String -> Comp s ()
info p m = _ifll LRInfo $ _log Info p m
warn p m = do
    f <- view (compileFlags . warnToError)
    if f then err p m else _ifll LRWarn $ _log Warn p m

_ifll :: LRLevel -> Comp s () -> Comp s ()
_ifll min pass = (< min) <$> view logLevel >>= flip unless pass

info', warn', infoln, warnln :: Phase -> [String] -> Comp s ()
info'  = unword2nd  info
warn'  = unword2nd  warn
infoln = unlines2nd info
warnln = unlines2nd warn

-- | 'infoP': Logs msg with 'Info' level during a 'Phase' with a header before.
infoP :: Show a => Phase -> String -> a -> Comp s ()
infoP p header x = info p header >> info p (show x)

-- | '_log': inserts a log item into a computation.
_log :: LogLevel -> Phase -> String -> Comp s ()
_log l p m = tell [LogItem l p m]

unword2nd, unlines2nd :: (a -> String -> b) -> a -> [String] -> b
unword2nd  f a b = f a $ unwords b
unlines2nd f a b = f a $ unlines b

--------------------------------------------------------------------------------
-- Log outputting:
--------------------------------------------------------------------------------

-- | 'bracketize': puts a 'Show'able in brackets.
bracketize :: Show x => x -> String
bracketize x = "[" ++ show x ++ "]"

-- | 'showLogItem': pretty "prints" a 'LogItem'.
showLogItem :: LogItem -> String
showLogItem (LogItem lvl phase msg) =
    unwords [bracketize lvl, bracketize phase ++ ":", prettify msg]

-- | 'showLogs': pretty prints an 'InfoLog'.
showLogs :: InfoLog -> String
showLogs = unlines . fmap showLogItem

-- | 'printLogs': pretty prints an 'InfoLog' to standard output.
printLogs :: InfoLog -> IO ()
printLogs = putStrLn . showLogs

-- | 'showError': pretty prints an 'ErrorMsg'.
showError :: ErrMsg -> String
showError (ErrMsg phase msg) = unwords ["ERROR!", bracketize phase, msg]

-- | 'printError': pretty prints an 'ErrorMsg' to standard output.
printError :: ErrMsg -> IO ()
printError = putStrLn . showError