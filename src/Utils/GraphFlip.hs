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
Module      : Frontend.GraphFlip
Description : Graph utilities for FGL.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Graph utilities for FGL.
-}
{-# LANGUAGE FlexibleInstances #-}

module Utils.GraphFlip where

import Data.Maybe
import Data.Foldable
import Data.Monoid
import Data.Traversable

import qualified Data.Graph.Inductive as G
import qualified Utils.Graph as UG

import Control.Arrow

import Utils.Pointless
import Utils.Foldable
import Utils.Function
import Utils.Monad

newtype Flip f b a = Flip { unFlip :: f a b }
    deriving (Eq, Ord, Show, Read)

inFlip :: (f a b -> f a' b') -> Flip f b a -> Flip f b' a'
inFlip g = Flip . g . unFlip

instance Foldable    (Flip G.Gr b) where foldMap = foldMapDefault
instance Functor     (Flip G.Gr b) where fmap    = fmapDefault
instance Traversable (Flip G.Gr b) where
    traverse f gr = (flip mkGraph (labEdges gr) . zip ns) <$> traverse f as
        where (ns, as) = unzip $ labNodes gr

empty :: G.Graph gr => Flip gr b a
empty = Flip G.empty

mkGraph :: G.Graph gr => [G.LNode a] -> [G.LEdge b] -> Flip gr b a
mkGraph = Flip .| G.mkGraph

labs :: G.Graph gr => Flip gr b a -> [a]
labs = labNodes >$> snd

labNodes :: G.Graph gr => Flip gr b a -> [G.LNode a]
labNodes = G.labNodes . unFlip

labEdges :: G.Graph gr => Flip gr b a -> [G.LEdge b]
labEdges = G.labEdges . unFlip

cyclesIn :: G.DynGraph gr => Flip gr b a -> [[G.LNode a]]
cyclesIn = UG.cyclesIn . unFlip

topsort' :: G.DynGraph gr => Flip gr b a -> [a]
topsort' = G.topsort' . unFlip

lab :: G.Graph gr => Flip gr b a -> G.Node -> Maybe a
lab = G.lab . unFlip

addLabels :: G.Graph g => Flip g b a -> [G.Node] -> [G.LNode a]
addLabels = UG.addLabels . unFlip

bfs :: G.Graph gr => G.Node -> Flip gr b a -> [G.Node]
bfs = G.bfs .$ unFlip

bfsL :: G.Graph gr => G.Node -> Flip gr b a -> [G.LNode a]
bfsL n gr = addLabels gr $ bfs n gr

lab' :: G.Graph gr => Flip gr b a -> G.Node -> a
lab' = fromJust .| lab

nmap :: G.Graph gr => Flip gr b a -> (a -> c) -> Flip gr b c
nmap = lnmap .$ second

lnmap :: G.Graph gr => Flip gr b a -> (G.LNode a -> G.LNode c) -> Flip gr b c
lnmap gr f = reconstr gr $ f <$> labNodes gr

gaction :: (Monad m, G.Graph gr)
        => Flip gr b a -> ([G.LNode a] -> m [G.LNode c]) -> m (Flip gr b c)
gaction gr f = reconstr gr <$> f (labNodes gr)

reconstr :: G.Graph gr => Flip gr b a -> [G.LNode c] -> Flip gr b c
reconstr gr ns = mkGraph ns $ labEdges gr

rootBfsM :: (Monad m, G.Graph gr, Applicative f, Monoid (f G.Node))
         => Flip gr b a -> z -> f G.Node -> (z -> f G.Node -> G.Node -> m z) -> m z
rootBfsM gr z i f = foldM' z (classGraphRoots gr $ labNodes gr) g
    where g z1 r = ancestralBfsM gr i r f z1

outdeg :: G.Graph gr => Flip gr b a -> G.Node -> Int
outdeg = G.outdeg . unFlip

pre :: G.Graph gr => Flip gr b a -> G.Node -> [G.Node]
pre = G.pre . unFlip

connected :: G.Graph gr => Flip gr e v -> G.Node -> G.Node -> Bool
connected gr x y = x `elem` (fst <$> bfsL y gr)

classGraphRoots :: G.Graph gr => Flip gr b a -> [G.LNode a] -> [G.Node]
classGraphRoots gr = filter ((0 ==) . outdeg gr) . (fst <$>)

-- | 'ancestralBfsM': monadic mapping of a graph using a BFS algorithm starting
-- from a specific node. The ancestors of the current node, and the current node
-- itself are passed to a monadic action producing a value that is ignored.
-- The algorithm assumes there are no cycles.
ancestralBfsM_ :: (Monad m, G.Graph gr, Applicative f, Monoid (f G.Node))
              => Flip gr b a -> f G.Node -> G.Node
              -> (f G.Node -> G.Node -> m ()) -> m ()
ancestralBfsM_ gr ancs curr _do =
    let ancs' = ancs <> pure curr in forM_ (pre gr curr) $ \child ->
        _do ancs' child >> ancestralBfsM_ gr ancs' child _do

-- | 'ancestralBfsM': monadic folding of a graph using a BFS algorithm starting
-- from a specific node. The current value, the ancestors of the current node,
-- and the current node itself are passed to a monadic action which produces
-- the next current value which is finally yielded as the result.
-- The algorithm assumes there are no cycles.
ancestralBfsM :: (Monad m, G.Graph gr, Applicative f, Monoid (f G.Node))
              => Flip gr b a -> f G.Node -> G.Node
              -> (z -> f G.Node -> G.Node -> m z) -> z -> m z
ancestralBfsM gr ancs curr _do z0 =
    _do z0 ancs curr >>= flip3 foldlM (pre gr curr)
        (lsh3 (ancestralBfsM gr $ ancs <> pure curr) _do)