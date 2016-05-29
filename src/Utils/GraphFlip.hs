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
Module      : Frontend.Typecheck
Description : Type checker for Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Type checker for Javalette compiler.
-}
{-# LANGUAGE FlexibleInstances #-}

module Utils.GraphFlip where

import Data.Traversable

import qualified Data.Graph.Inductive as G
import qualified Utils.Graph as UG

import Utils.Pointless

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

labNodes :: G.Graph gr => Flip gr b a -> [G.LNode a]
labNodes = G.labNodes . unFlip

labEdges :: G.Graph gr => Flip gr b a -> [G.LEdge b]
labEdges = G.labEdges . unFlip

cyclesIn :: G.DynGraph gr => Flip gr b a -> [[G.LNode a]]
cyclesIn = UG.cyclesIn . unFlip

topsort' :: G.DynGraph gr => Flip gr b a -> [a]
topsort' = G.topsort' . unFlip

lab :: G.DynGraph gr => Flip gr b a -> G.Node -> Maybe a
lab gr = G.lab (unFlip gr)

addLabels :: G.Graph g => Flip g b a -> [G.Node] -> [G.LNode a]
addLabels = UG.addLabels . unFlip

bfs :: G.Graph gr => G.Node -> Flip gr b a -> [G.Node]
bfs n = G.bfs n . unFlip

bfsL :: G.Graph gr => G.Node -> Flip gr b a -> [G.LNode a]
bfsL n gr = addLabels gr $ bfs n gr