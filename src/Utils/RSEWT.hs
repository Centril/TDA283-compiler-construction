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
Module      : Utils.RSEWT
Description : RSEWT monad transformer.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

RSEWT monad transformer.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module Utils.RSEWT (
    -- * Types
    RSEW, RSEWT,

    -- ** Running
    runRSEWT, runRSEW, evalRSEWT,

    -- ** Monad stack transformations
    rebase, io, transST, changeST, ($:<<)
) where

import Control.Arrow
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Morph

import Utils.Pointless

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

-- | 'evalRSEWT': evaluates a 'RSEWT' computation given initial states.
evalRSEWT :: Functor m => RSEWT s r e w m a -> r -> s -> m (Either e a, w)
evalRSEWT ev = (first (fst <$>) <$>) .| runRSEWT ev

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

-- | 'io': alias of 'liftIO'
io :: MonadIO m => IO a -> m a
io = liftIO