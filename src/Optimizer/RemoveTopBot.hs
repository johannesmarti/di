module Optimizer.RemoveTopBot (
  removeTop,
  removeBot,
) where

import Data.Map.Strict as Map
import Data.Set as Set

import Graph

backwardsElementFlow :: (Ord v, Ord n) =>
                        Graph r v n -> (Unfolding r v n -> Bool) -> Set n
backwardsElementFlow graph isStarting = let
    m = dataMap graph
    startingSet = Map.keysSet (Map.filter (isStarting . unfolding) m)
    startingQueue = Set.toList startingSet
    worker accum [] = accum
    worker accum (next:remaining) = let
        preds = predecessorsOfNode graph next
        isCovered n = case unfoldNode graph n of
                        And succs -> succs `isSubsetOf` accum
                        _         -> True
        toBeAdded n = n `Set.notMember` accum && isCovered n
        toAdd = Set.filter toBeAdded preds
        newAccum = toAdd `Set.union` accum
        newQueue = Set.toList toAdd ++ remaining
      in worker newAccum newQueue
  in worker startingSet startingQueue

-- this function computes all nodes that are top and then keeps all others
removeTop :: (Ord v, Ord n) => Graph r v n -> Graph r v n
removeTop graph = let
    isStarting (And succs) = Set.null succs
    isStarting _           = False
    topNodes = backwardsElementFlow graph isStarting
  in subgraphOnPredicate (`Set.notMember` topNodes) graph

-- this function computes the least-fixpoint of all nodes that are not bot
removeBot :: (Ord v, Ord n) => Graph r v n -> Graph r v n
removeBot graph = let
    isStarting (Atom _)       = True
    isStarting (Equality _ _) = True
    isStarting (And succs)    = Set.null succs
    isStarting _              = False
    nonBotNodes = backwardsElementFlow graph isStarting
  in subgraphOnSubset nonBotNodes graph

