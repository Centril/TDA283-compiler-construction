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
    (<!>), (<:>), (<<$>), (<>$>), maybeErr
) where

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