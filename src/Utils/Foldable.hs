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
Module      : Utils.Foldable
Description : General utility functions on Foldable:s, Lists, Monoids.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

General utility functions on Foldable:s, Lists, Monoids.
-}
module Utils.Foldable (
    -- * Operations
    mfind, mfindU, maybePred, modifyf, addSuffixes
) where

import Data.Monoid
import Data.Maybe
import Data.List

import Control.Arrow

-- 'maybePred': transforms a function into a predicate with the semantics that:
-- if the function returns 'Just', then there's a match, otheriwse there's not.
maybePred :: (a -> Maybe b) -> a -> Bool
maybePred p = isJust . p

-- | 'mfind': generalizes 'find' taking a "predicate" using 'Maybe' where
-- 'Nothing' rejects the element and 'Just' accepts it. It then it maybe finds
-- the leftmost element mapped, or 'Nothing' if all elements were rejected.
mfind :: Foldable t => (a -> Maybe b) -> t a -> Maybe b
mfind p = getFirst . foldMap (First . p)

-- | 'mfindU': specializes 'mfind' for lists while also allowing an update to
-- the place the element was considered found. The updated list is yielded
-- alongside the potenially found transformed element as a pair.
mfindU :: (a -> Maybe (b, a)) -> [a] -> (Maybe b, [a])
mfindU tp = maybe (Nothing, []) f . uncons
    where f (a, as) = maybe (second (a:) $ mfindU tp as) (Just *** (:as)) (tp a)

-- | 'modifyf': modifies the first element of a list.
modifyf :: (t -> t) -> [t] -> [t]
modifyf _ []       = []
modifyf f (x : xs) = f x : xs

-- | 'addSuffixes': given a structure with suffixes, prepends a prefix and a
-- separator. The prepending is done with the semantics of a monoid.
addSuffixes :: (Functor f, Monoid a) => f a -> a -> a -> f a
addSuffixes ss j pre = (pre' `mappend`) <$> ss
    where pre' = pre `mappend` j