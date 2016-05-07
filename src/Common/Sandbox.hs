--------------------------------------------------------------------------------
-- Copyright   :  (C) 2008 Edward Kmett, 2016 Mazdak Farrokhzad
-- License     :  BSD-style (see the file LICENSE)
--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances,
             UndecidableInstances, MultiParamTypeClasses #-}

module Common.Sandbox where

import Control.Applicative(Alternative)
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer

--------------------------------------------------------------------------------
-- IxFunctor:
--------------------------------------------------------------------------------

class IxFunctor f where
    (#<$>) :: (a -> b) -> f j k a -> f j k b

infixl 4 #<$>

imap :: IxFunctor f => (a -> b) -> f j k a -> f j k b
imap = (#<$>)

instance IxFunctor f => Functor (f j k) where
    fmap = (#<$>)

--------------------------------------------------------------------------------
-- IxPointed, IxCopointed:
--------------------------------------------------------------------------------

class IxFunctor m => IxPointed m where
    ireturn :: a -> m i i a

class IxFunctor w => IxCopointed w where
    iextract :: w i i a -> a

--------------------------------------------------------------------------------
-- IxApplicative:
--------------------------------------------------------------------------------

class IxPointed f => IxApplicative f where
    (#<*>) :: f i j (a -> b) -> f j k a -> f i k b

infixl 4 #<*>

iliftA :: IxApplicative m => (a -> b) -> m j k a -> m j k b
iliftA = (#<*>) . ireturn

instance IxApplicative f => IxFunctor f where
    (#<$>) = iliftA

instance IxApplicative f => Applicative (f j j) where
    pure  = ireturn
    (<*>) = (#<*>)

--------------------------------------------------------------------------------
-- IxMonad:
--------------------------------------------------------------------------------

class IxApplicative m => IxMonad m where
    (#>>=) :: m i j a -> (a -> m j k b) -> m i k b

(#=<<) :: IxMonad m => (a -> m j k b) -> m i j a -> m i k b
(#=<<) = flip (#>>=)

(#>>) :: IxMonad m => m i j a -> m j k b -> m i k b
(#>>) mx = (mx #>>=) . const

(#>=>) :: IxMonad m => (a -> m i j b) -> (b -> m j k c) -> a -> m i k c
(#>=>) f g a = f a #>>= g

(#<=<) :: IxMonad m => (b -> m j k c) -> (a -> m i j b) -> a -> m i k c
(#<=<) = flip (#>=>)

infixl 1 #>>=, #>>
infixr 1 #=<<, #>=>, #<=<

ibind :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
ibind = (#>>=)

ijoin :: IxMonad m => m i j (m j k a) -> m i k a 
ijoin = (#=<<) id

iliftM :: IxMonad m => (a -> b) -> m j k a -> m j k b
iliftM f m = m #>>= ireturn . f

iliftM2 :: IxMonad m => (a -> b -> c) -> m i j a -> m j k b -> m i k c
iliftM2 f m1 m2 = f #<$> m1 #<*> m2

iliftM3 :: IxMonad m => (a -> b -> c -> d)
        -> m i j a -> m j k b -> m k l c -> m i l d
iliftM3 f m1 m2 m3 = iliftM2 f m1 m2 #<*> m3

iliftM4 :: IxMonad m => (a -> b -> c -> d -> e)
        -> m i j a -> m j k b -> m k l c -> m l n d -> m i n e
iliftM4 f m1 m2 m3 m4 = iliftM3 f m1 m2 m3 #<*> m4

iliftM5 :: IxMonad m => (a -> b -> c -> d -> e -> f)
        -> m i j a -> m j k b -> m k l c -> m l n d -> m n o e -> m i o f
iliftM5 f m1 m2 m3 m4 m5 = iliftM4 f m1 m2 m3 m4 #<*> m5

iap :: IxMonad m => m i j (a -> b) -> m j k a -> m i k b
iap mf m = mf #>>= \f -> m #>>= ireturn . f

instance IxMonad m => IxApplicative m where
    (#<*>) = iap

instance IxMonad m => Monad (m j j) where
    return = ireturn
    (>>=)  = (#>>=)

--------------------------------------------------------------------------------
-- IxMonadZero, IxMonadPlus:
--------------------------------------------------------------------------------

class IxMonad m => IxMonadZero m where
    imzero :: m i j a

class IxMonadZero m => IxMonadPlus m where
    implus :: m i j a -> m i j a -> m i j a

instance (IxMonadPlus m, Alternative (m j j)) => MonadPlus (m j j) where
    mzero = imzero
    mplus = implus

--------------------------------------------------------------------------------
-- IxMonadFix:
--------------------------------------------------------------------------------

class IxMonad m => IxMonadFix m where
    imfix :: (a -> m i i a) -> m i i a

instance IxMonadFix m => MonadFix (m i i) where
    mfix = imfix

--------------------------------------------------------------------------------
-- IxMonadTrans:
--------------------------------------------------------------------------------

class IxMonadTrans t where
    ilift :: Monad m => m a -> t m i i a 

--------------------------------------------------------------------------------
-- IxMonadState:
--------------------------------------------------------------------------------

class IxMonad m => IxMonadState m where
    iget :: m i i i
    iput :: j -> m i j ()

imodify :: IxMonadState m => (i -> j) -> m i j ()
imodify f = iget #>>= iput . f

igets :: IxMonadState m => (i -> a) -> m i i a
igets f = iget #>>= ireturn . f

instance IxMonadState m => MonadState i (m i i) where
    get = iget
    put = iput

--------------------------------------------------------------------------------
-- IxStateT:
--------------------------------------------------------------------------------

newtype IxStateT m i j a = IxStateT { runIxStateT :: i -> m (a, j) }

evalIxStateT :: Functor f => IxStateT f i j b -> i -> f b
evalIxStateT m i = fst <$> runIxStateT m i

execIxStateT :: Functor f => IxStateT f i j a -> i -> f j
execIxStateT m i = snd <$> runIxStateT m i

instance Monad m => IxPointed (IxStateT m) where
    ireturn a = IxStateT $ \s -> return (a, s)

instance Monad m => IxMonad (IxStateT m) where
     m #>>= k = IxStateT $ runIxStateT m >=> \ ~(a, o) -> runIxStateT (k a) o

instance Monad m => IxMonadState (IxStateT m) where
    iget   = IxStateT $ \s -> return (s, s)
    iput s = IxStateT $ \_ -> return ((), s)

instance MonadPlus m => IxMonadZero (IxStateT m) where
    imzero = IxStateT $ const mzero

instance MonadPlus m => IxMonadPlus (IxStateT m) where
    implus m n = IxStateT $ \s -> runIxStateT m s `mplus` runIxStateT n s

instance MonadFix m => IxMonadFix (IxStateT m) where
    imfix f = IxStateT $ \s -> mfix $ \ ~(a, _) -> runIxStateT (f a) s

instance IxMonadTrans IxStateT where
    ilift m = IxStateT $ \s -> m >>= \a -> return (a, s)

instance MonadIO m => MonadIO (IxStateT m i i) where
    liftIO = ilift . liftIO

instance MonadError e m => MonadError e (IxStateT m i i) where
    throwError = ilift . throwError
    m `catchError` h = IxStateT $ \s -> runIxStateT m s `catchError`
                                  \e -> runIxStateT (h e) s

instance MonadWriter w m => MonadWriter w (IxStateT m i i) where
    tell = ilift . tell
    listen m = IxStateT $ \s -> do 
        ~((a, s'), w) <- listen (runIxStateT m s)
        return ((a, w), s')
    pass m = IxStateT $ \s -> pass $ do
        ~((a, f), s') <- runIxStateT m s
        return ((a, s'), f)

--------------------------------------------------------------------------------
-- IxSEWT:
--------------------------------------------------------------------------------

runIxSEWT :: IxSEWT e w m i j a -> i -> m (Either e (a, j), w)
runIxSEWT ev e = runWriterT $ runExceptT $ runIxStateT (_runIxSEWT ev) e

newtype IxSEWT e w m i j a = IxSEWT {
    _runIxSEWT :: IxStateT (ExceptT e (WriterT w m)) i j a }
    deriving (IxPointed ,IxMonad, IxMonadState,
              IxMonadFix,IxMonadZero, IxMonadPlus)

instance Monoid w => IxMonadTrans (IxSEWT e w) where
    ilift = IxSEWT . ilift . lift . lift

instance (Monoid w, MonadIO m) => MonadIO (IxSEWT e w m i i) where
    liftIO = IxSEWT . liftIO

instance (Monoid w, Monad m) => MonadError e (IxSEWT e w m i i) where
    throwError = IxSEWT . throwError
    m `catchError` h = IxSEWT $ catchError (_runIxSEWT m) (_runIxSEWT . h)

instance (Monoid w, Monad m) => MonadWriter w (IxSEWT e w m i i) where
    tell = IxSEWT . tell
    listen = IxSEWT . listen . _runIxSEWT
    pass = IxSEWT . pass . _runIxSEWT

type Test i j a = IxSEWT Int [Int] Identity i j a
t0 :: Test i i a -> (a -> Test i i b) -> Test i i b
t0 = (>>=)
t1 :: Test i i (a -> b) -> Test i i a -> Test i i b
t1 = (<*>)
t2 :: Test i i i
t2 = get
t3 :: i -> Test i i ()
t3 = put
t4 :: (a -> Test i i a) -> Test i i a
t4 = mfix
t5 :: Int -> Test i i a
t5 = throwError
t6 :: [Int] -> Test i i ()
t6 = tell
t7 :: IO a -> IxSEWT Int [Int] IO i i a
t7 = liftIO