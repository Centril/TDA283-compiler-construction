{- Javalette Compiler
   Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -}

{-|
Module      : Utils.Foldable
Description : General utility functions for Foldable:s.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
Stability   : experimental
Portability : ALL

General utility functions for Foldable:s.
-}
module Utils.Foldable (
    -- * Operations
    mfind, maybePred
) where

import Data.Monoid
import Data.Maybe

-- 'maybePred': transforms a function into a predicate with the semantics that:
-- if the function returns 'Just', then there's a match, otheriwse there's not.
maybePred :: (a -> Maybe b) -> a -> Bool
maybePred p = isJust . p

-- | 'mfind': generalizes 'find' taking a "predicate" using 'Maybe' where
-- 'Nothing' rejects the element and 'Just' accepts it. It then it maybe finds
-- the leftmost element mapped, or 'Nothing' if all elements were rejected.
mfind :: Foldable t => (a -> Maybe b) -> t a -> Maybe b
mfind p = getFirst . foldMap (First . p)