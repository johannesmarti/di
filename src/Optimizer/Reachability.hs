module Optimizer.Reachability (
  keepReachable,
) where

import Data.Map.Strict as Map
import Data.Set as Set

import LogicalGraph

keepReachable :: (Ord v, Ord n) => [n] -> Graph r v n -> Graph r v n
keepReachable roots graph = let
    startingQueue = roots
    startingSet = Set.fromList roots
    worker accum [] = accum
    worker accum (next:remaining) = let
        succs = successorsOfNode graph next
        toBeAdded n = n `Set.notMember` accum
        toAdd = Set.filter toBeAdded succs
        newAccum = toAdd `Set.union` accum
        newQueue = Set.toList toAdd ++ remaining
      in worker newAccum newQueue
  in subgraphOnSubset (worker startingSet startingQueue) graph

