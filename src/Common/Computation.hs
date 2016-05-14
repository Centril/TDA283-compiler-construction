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
module Common.Computation (
    -- * Modules
    module X,

    -- * Types
    JlcTarget,
    Comp, CompEval, IOComp, IOCompEval, BaseComp,
    Phase(..), ErrMsg(..), LogLevel, InfoLog, LogItem(..),

    -- * Operations

    -- ** Running computations
    evalIOComp,

    -- ** Logging and Error operations
    warn, warn', warnln, info, info', infoln, infoP, err, err', errln,
    showLogItem, showLogs, printLogs, showError, printError, phaseEnd
) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer

import Control.Lens (view)

import Utils.RSEWT as X
import Utils.Pointless
import Utils.Terminal

import Common.Options as X

--------------------------------------------------------------------------------
-- Jlc targets:
--------------------------------------------------------------------------------

type JlcTarget = JlcOptions -> IOCompEval ()

--------------------------------------------------------------------------------
-- Computations:
--------------------------------------------------------------------------------

type BaseComp m s = RSEWT s JlcOptions ErrMsg InfoLog m

-- | 'Comp': A computation given an environment or state s,
-- potential fail-fast errors of type 'ErrMsg' and accumulated 'InfoLog's.
-- See 'CompResult' for details.
type Comp s = BaseComp Identity s

-- | 'CompEval': result of an evaluation of a 'Comp' computation.
type CompEval a = (Either ErrMsg a, InfoLog)

--------------------------------------------------------------------------------
-- IO Computations:
--------------------------------------------------------------------------------

-- | 'IOComp': like 'Comp' but with 'IO' instead of 'Identity' as base monad.
type IOComp s = BaseComp IO s

-- | 'IOCompEval': result of an evaluation of an 'IOComp' computation.
type IOCompEval a = IO (Either ErrMsg a, InfoLog)

-- | 'evalIOComp': Evaluates the entire 'IOComp' computation given a state to
-- work inside as starting environment / state.
evalIOComp :: IOComp s a -> JlcOptions -> s -> IOCompEval a
evalIOComp = evalRSEWT

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

errln, err' :: Monad m => Phase -> [String] -> BaseComp m s a
errln = unlines2nd err
err'  = unword2nd err

-- | 'err': throws an error in a computation ignoring any continuations.
err :: Monad m => Phase -> String -> BaseComp m s a
err = throwError .| ErrMsg

warn, info :: Monad m => Phase -> String -> BaseComp m s ()
info p m = _ifll LRInfo $ _log Info p m
warn p m = do
    f <- view (compileFlags . warnToError)
    if f then err p m else _ifll LRWarn $ _log Warn p m

_ifll :: Monad m => LRLevel -> BaseComp m s () -> BaseComp m s ()
_ifll _min _pass = (< _min) <$> view logLevel >>= flip unless _pass

info', warn', infoln, warnln :: Monad m => Phase -> [String] -> BaseComp m s ()
info'  = unword2nd  info
warn'  = unword2nd  warn
infoln = unlines2nd info
warnln = unlines2nd warn

-- | 'infoP': Logs msg with 'Info' level during a 'Phase' with a header before.
infoP :: (Monad m, Show a) => Phase -> String -> a -> BaseComp m s ()
infoP p header x = info p header >> info p (show x)

-- | '_log': inserts a log item into a computation.
_log :: Monad m => LogLevel -> Phase -> String -> BaseComp m s ()
_log l p m = tell [LogItem l p m]

unword2nd, unlines2nd :: (a -> String -> b) -> a -> [String] -> b
unword2nd  f a b = f a $ unwords b
unlines2nd f a b = f a $ unlines b

phaseEnd :: (Monad m, Show s, Show a) => Phase -> a -> BaseComp m s a
phaseEnd p x = do
    get >>= infoP p "Final environment value:"
    infoP p "Final computed value:" x >> return x

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