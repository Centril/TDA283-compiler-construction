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
Module      : Utils.Monad
Description : General monadic, applicative, functor utility functions.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

General monadic, applicative, functor utility functions.
-}
module Utils.Monad (
    -- * Functor operations
    (>$>), (<$<), (<<$>), (<>$>), (<$$>), fkeep,

    -- * Applicative operations
    (<!>), (<:>), (<++>),

    -- * Monad operations
    (>?=>), (>=?>), (<<=), (<<=>), (.>>), (>>.),
    maybeErr, unless', foldl1M, foldr1M, untilEqM, untilMatchM
) where

import Data.Foldable

import Control.Monad

-- | '<$<': Left-to-right "Kleisli" composition of 'Functor's.
(>$>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
(>$>) = flip (<$<)
infixr 3 >$>

-- | '<$<': Right-to-left "Kleisli" composition of 'Functor's.
(<$<) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(<$<) = (.) . (<$>)
infixr 3 <$<

-- | '>?=>': Same as '>=>', i.e: Left-to-right Kleisli composition of monads.
-- BUT: first it applies something to the left hand side.
(>?=>) :: Monad m => (t -> a -> m b) -> (b -> m c) -> t -> a -> m c
(>?=>) m1 m2 x = m1 x >=> m2
infixr 1 >?=>

-- | '>=?>': Same as '>=>', i.e: Left-to-right Kleisli composition of monads.
-- BUT: first it applies something to the right hand side.
(>=?>) :: Monad m => (a -> m b) -> (t -> b -> m c) -> t -> a -> m c
(>=?>) m1 m2 x = m1 >=> m2 x
infixr 1 >=?>

-- | '.>>': blinding left to right Kleisli post-composition of monads.
-- Is to '>=>' what '>>' is to '>>='.
(.>>) :: Monad m => (a -> m b) -> m c -> a -> m c
(.>>) fb c a = fb a >> c

-- | '>>.': blinding left to right Kleisli pre-composition of monads.
-- Is to '>=>' what '>>' is to '>>='.
(>>.) :: Monad m => m a -> (b -> m c) -> b -> m c
(>>.) ma fc b = ma >> fc b

-- | '<<=': sequentially compose two actions, passing value produced by first as
-- an argument to the second, but returning the value of produced by first.
(<<=) :: Monad m => m a -> (a -> m b) -> m a
(<<=) m f = m >>= \x -> f x >> return x
infixl 5 <<=

-- | '<<=>': "Kleisli" version of '<<='.
(<<=>) :: Monad m => (a -> m b) -> (a -> m c) -> a -> m c
(<<=>) f g a = f a >> g a
infixl 5 <<=>

-- | '<!>': sequential application of a non-applicative value
-- lifted into the same 'Applicative' of as the function applied.
(<!>) :: Applicative f => f (a -> b) -> a -> f b
(<!>) f = (<*>) f . pure
infixr 7 <!>

-- | '<!>': version of 'fmap' where non-applicative value is first lifted
-- purely into the 'Applicative'.
(<:>) :: Applicative f => (a -> b) -> a -> f b
(<:>) f = fmap f . pure
infixr 8 <:>

-- | '<<$>': binary operator that performs 'fmap' for a 'Functor' f where the
-- given value is a pair. This version of 'fmap' extracts 'fst' part of tuple.
(<<$>) :: Functor f => (a -> c) -> f (a, b) -> f c
f <<$> y = f . fst <$> y

-- | '<>$>': binary operator that performs 'fmap' for a 'Functor' f where the
-- given value is a pair. This version of 'fmap' extracts 'snd' part of tuple.
(<>$>) :: Functor f => (a -> c) -> f (a, b) -> f c
f <>$> y = f . fst <$> y

-- | 'maybeErr': if the given 'Maybe' is 'Nothing' the monadic action
-- whenNothing is run, otherwise the value in 'Just' is 'return':ed.
maybeErr :: Monad m => m a -> Maybe a -> m a
maybeErr whenNothing = maybe whenNothing return

-- | '<$$>': alias for composition of fmap with itself.
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = (<$>) . (<$>)

-- | 'unless'': sequentially composes first argument with a check where the
-- value is given to a predicate (in second argument). If the predicate holds,
-- then its given value is returned, else the function in the third argument is
-- given the value and is the result of the computation.
unless' :: Monad m => m a -> (a -> Bool) -> (a -> m a) -> m a
unless' m p e = m >>= \x -> if p x then return x else e x

-- | 'fkeep': given a function that produces f b given an a. And given an a in
-- the second argument, a functor with both values as a pair is produced.
fkeep :: Functor f => (a -> f b) -> a -> f (a, b)
fkeep f a = (\b -> (a, b)) <$> f a

-- | '<++>': 'mappend' a monoidal value inside a monad to another.
(<++>) :: (Applicative f, Monoid b) => f b -> f b -> f b
(<++>) l r = mappend <$> l <*> r

-- | 'foldl1M': variant of 'foldlM' without base case, so non-empty structure.
foldl1M :: (Foldable t, Monad m) => (a -> a -> m a) -> t a -> m a
foldl1M f t = let (z:xs) = toList t in foldlM f z xs

-- | 'foldr1M': variant of 'foldrM' without base case, so non-empty structure.
foldr1M :: (Foldable t, Monad m) => (a -> a -> m a) -> t a -> m a
foldr1M f t = let (z:xs) = toList t in foldrM f z xs

-- | 'untilEqM': same as 'untilEq' but in a monadic context.
untilEqM :: (Eq a, Monad m) => (a -> m a) -> m a -> m a
untilEqM = untilMatchM (==)

-- | 'untilMatchM': same as 'untilMatch' but in a monadic context.
untilMatchM :: Monad m => (a -> a -> Bool) -> (a -> m a) -> m a -> m a
untilMatchM p f = (>>= \x -> unless' (f x) (p x) (untilMatchM p f . return))