module Optimizer.NonEmptyAnalysis (
  removeEmpty,
) where

import Data.Map.Strict as Map
import Data.Set as Set

import Graph

removeEmpty :: (Ord v, Ord n) => Graph r v n -> Graph r v n
removeEmpty graph = let
    isStarting (Atom _)       = True
    isStarting (Equality _ _) = True
    isStarting (And succs)    = Set.null succs
    isStarting _              = False
    m = dataMap graph
    startingSet = Map.keysSet (Map.filter (isStarting . unfolding) m)
    startingQueue = Set.toList startingSet
    worker accum [] = accum
    worker accum (next:remaining) = let
        preds = predecessorsOfNode graph next
        isCovered n = case unfoldNode graph n of
                        (And succs) -> succs `isSubsetOf` accum
                        _           -> True
        toBeAdded n = n `Set.notMember` accum && isCovered n
        toAdd = Set.filter toBeAdded preds
        newAccum = toAdd `Set.union` accum
        newQueue = Set.toList toAdd ++ remaining
      in worker newAccum newQueue
  in subgraphOnSubset (worker startingSet startingQueue) graph where

