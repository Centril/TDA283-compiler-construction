{- |
   Module      : Utils.Graph (Data.Graph.Analysis.Algorithms.Common)
   Description : Algorithms for all graph types.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   Defines algorithms that work on both undirected and
   directed graphs.
 -}
module Utils.Graph (
    -- * Cycle Detection
    cyclesIn, addLabels
) where

import Data.Graph.Inductive.Graph

import Data.Function (on)
import Data.List (unfoldr, nubBy)
import Data.Maybe (fromJust)

import Control.Monad(ap)
import Control.Arrow (first)

-- -----------------------------------------------------------------------------
{- $cycles
   Cycle detection.  Find cycles by finding all paths from a given
   node, and seeing if it reaches itself again.
 -}

-- | Find all cycles in the given graph.
cyclesIn   :: (DynGraph g) => g a b -> [[LNode a]]
cyclesIn g = map (addLabels g) (cyclesIn' g)

-- | Find all cycles in the given graph, returning just the nodes.
cyclesIn' :: (DynGraph g) => g a b -> [[Node]]
cyclesIn' = concat . unfoldr findCycles . mkSimple

-- | Find all cycles containing a chosen node.
findCycles :: (DynGraph g) => g a b -> Maybe ([[Node]], g a b)
findCycles g
    | isEmpty g = Nothing
    | otherwise = Just . getCycles . matchAny $ g
    where getCycles (ctx, g') = (cyclesFor (ctx, g'), g')

-- | Find all cycles for the given node.
cyclesFor :: (DynGraph g) => GDecomp g a b -> [[Node]]
cyclesFor = map init . filter isCycle . pathTree . first Just
    where isCycle p = not (single p) && (head p == last p)

-- -----------------------------------------------------------------------------

-- | Find all possible paths from this given node, avoiding loops,
--   cycles, etc.
pathTree             :: (DynGraph g) => Decomp g a b -> [[Node]]
pathTree (Nothing,_) = []
pathTree (Just ct,g)
    | isEmpty g = []
    | null sucs = [[n]]
    | otherwise = (:) [n] . map (n:) . concatMap (subPathTree g') $ sucs
    where n = node' ct
          sucs = suc' ct
          -- Avoid infinite loops by not letting it continue any further
          ct' = makeLeaf ct
          g' = ct' & g
          subPathTree gr n' = pathTree $ match n' gr

-- | Remove all outgoing edges
makeLeaf           :: Context a b -> Context a b
makeLeaf (p,n,a,_) = (p', n, a, [])
    where -- Ensure there isn't an edge (n,n)
          p' = filter (\(_,n') -> n' /= n) p

-- | Return true if and only if the list contains a single element.
single     :: [a] -> Bool
single [_] = True
single  _  = False

-- | Obtain the labels for a list of 'Node's.
--   It is assumed that each 'Node' is indeed present in the given graph.
addLabels    :: (Graph g) => g a b -> [Node] -> [LNode a]
addLabels gr = map (ap (,) (fromJust . lab gr))

-- | Makes the graph a simple one, by removing all duplicate edges and loops.
--   The edges removed if duplicates exist are arbitrary.
mkSimple :: (DynGraph gr) => gr a b -> gr a b
mkSimple = gmap simplify
    where rmLoops n = filter ((/=) n . snd)
          rmDups = nubBy ((==) `on` snd)
          simpleEdges n = rmDups . rmLoops n
          simplify (p, n, l, s) = (simpleEdges n p, n, l, simpleEdges n s)