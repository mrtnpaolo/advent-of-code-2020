module Advent.Search where

import qualified Advent.Queue as Queue
import qualified Data.Set as Set

{-# INLINE bfs #-}
bfs :: Ord a => (a -> [a]) -> a -> [a]
bfs = bfsOn id

-- | Enumerate the reachable states in breadth-first order
-- given a successor state function and initial state.
--
-- States are compared for equality using the representative
-- function. If the representatives are equal the state is
-- considered already visited.
{-# INLINE [0] bfsOn #-}
bfsOn ::
  Ord r =>
  (a -> r)   {- ^ representative function   -} ->
  (a -> [a]) {- ^ successor state generator -} ->
  a          {- ^ initial state             -} ->
  [a]        {- ^ reachable states          -}
bfsOn rep next start = bfsOnN rep next [start]

{-# INLINE [0] bfsOnN #-}
bfsOnN ::
  Ord r =>
  (a -> r)   {- ^ representative function   -} ->
  (a -> [a]) {- ^ successor state generator -} ->
  [a]        {- ^ initial state             -} ->
  [a]        {- ^ reachable states          -}
bfsOnN rep next start = loop Set.empty (Queue.fromList start)
  where
    loop _ Queue.Empty = []
    loop seen (x Queue.:<| q1)
      | Set.member r seen =     loop seen  q1
      | otherwise         = x : loop seen1 q2
      where
        r     = rep x
        seen1 = Set.insert r seen
        q2    = Queue.appendList (next x) q1
