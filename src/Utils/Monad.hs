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
    -- * Operations
    (>?=>), (>=?>), (<<=), (<!>), (<:>),
    (<<$>), (<>$>), maybeErr, (<$$>), unless'
) where

import Control.Monad

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

-- | '<<=': sequentially compose two actions, passing value produced by first as
-- an argument to the second, but returning the value of produced by first.
(<<=) :: Monad m => m a -> (a -> m b) -> m a
(<<=) m f = m >>= \x -> f x >> return x
infixl 5 <<=

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