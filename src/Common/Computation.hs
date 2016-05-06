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
    -- * Types
    Comp, CompResult,
    Phase(..), ErrMsg(..), LogLevel, InfoLog, LogItem(..),

    -- * Operations

    -- ** Running computations
    runComp, transST, changeST, ($:<<),

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

import Utils.Pointless
import Utils.Terminal

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
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO,
              MonadState s, MonadError e, MonadWriter w)

-- 'runSEW': runs a 'SEW' computation given an initial state, specialization of
-- 'runSEWT' for the 'Identity' base monad.
runSEW :: SEWT s e w Identity a -> s -> (Either e (a, s), w)
runSEW ev e = runIdentity $ runSEWT ev e

-- | 'runSEWT': runs a 'SEWT' computation given an initial state.
runSEWT :: SEWT s e w m a -> s -> m (Either e (a, s), w)
runSEWT ev e = runWriterT $ runExceptT $ runStateT (_runSEWT ev) e

-- | 'transST': transitions between one 'SEWT' computation with state x, to
-- another with state y. The current state and result of the given computation
-- is given to a mapping function that must produce the next computation.
-- The initial state must also be passed as the last parameter.
transST :: Functor m
        => (((a, y), x) -> (a, y)) -> SEWT x e w m a -> x -> SEWT y e w m a
transST f x_c x_i = SEWT $ StateT $ \y_i -> ExceptT . WriterT $
    first ((\(a, x_f) -> f ((a, y_i), x_f)) <$>) <$> runSEWT x_c x_i

-- | '$:<<': infix version 'transST'
($:<<) :: Functor m
       => (((a, y), x) -> (a, y)) -> SEWT x e w m a -> x -> SEWT y e w m a
($:<<)  = transST

-- 'changeST': specialization of 'transST', ignoring final state of the
-- first computation directly using the initial of the second computation.
changeST :: (Functor m)
         => SEWT x e w m a -> x -> SEWT y e w m a
changeST =  transST fst

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
warn = _log Warn
info = _log Info

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

unword2nd, unlines2nd :: (t1 -> String -> t) -> t1 -> [String] -> t
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
    concat [bracketize lvl, bracketize phase, ": ", prettify msg]

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