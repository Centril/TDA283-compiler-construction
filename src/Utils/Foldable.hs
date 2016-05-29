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
    mfind, mfindU, maybePred, modifyf, addSuffixes, nubDupsBy, fromKVL,
    foldM', foldM2, maybe3, insert3, iinsert3
) where

import Data.Foldable (toList)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Monoid
import Data.Maybe
import Data.List

import Control.Arrow
import Control.Monad

import Utils.Pointless
import Utils.Function

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

-- | 'duplicatesBy': for a given list yields a pair where the fst contains the
-- the list without any duplicates, and snd contains the duplicate elements.
-- This is determined by a user specified binary predicate function.
nubDupsBy :: (a -> a -> Bool) -> [a] -> ([a], [a])
nubDupsBy p = foldl f ([], [])
    where f (seen, dups) x | any (p x) seen = (seen, dups ++ [x])
                           | otherwise      = (seen ++ [x], dups)

-- | 'fromKVL': creates, from two foldables (assumed to be of the same size),
-- a lazy map from the first to the second.
fromKVL :: (Foldable f, Ord k) => f k -> f v -> M.Map k v
fromKVL fk fv = M.fromList $ zip (toList fk) (toList fv)

-- | foldM': 'foldM' with first argument last.
foldM' :: (Monad m, Foldable t) => b -> t a -> (b -> a -> m b) -> m b
foldM' z t f = foldM f z t

-- | 'foldM2': fold over 2 structures simultaneously.
foldM2 :: (Monad m, Foldable t1, Foldable t2)
       => z -> t1 x1 -> t2 x2 -> (z -> x1 -> x2 -> m z) -> m z
foldM2 z0 t1 t2 f = foldM' z0 t1 $ \z1 x1 -> foldM' z1 t2 (f `flip` x1)

-- | 'maybe3': 'maybe' with the 'Maybe' as first argument.
maybe3 :: Maybe a -> r -> (a -> r) -> r
maybe3  = flip3 maybe

-- | 'insert3': 'insert' for 'Map' with map as first argument.
insert3 :: Ord k => M.Map k a -> k -> a -> M.Map k a
insert3 = flip3 M.insert

-- | 'insert3': 'insert' for 'IntMap' with map as first argument.
iinsert3 :: IM.IntMap a -> IM.Key -> a -> IM.IntMap a
iinsert3 = flip3 IM.insert