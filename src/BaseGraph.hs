module BaseGraph (
  unfolding,
  Graph,
  dataMap,
  domain,
  unfoldNode,
  successorsOfNode,
  predecessorsOfNode,
  inputVariablesFromSuccessors,
  isCoherent,
  fromTupleList,
  BaseGraph.fromSet,
  fromMap,
  subgraphOnSubset,
  subgraphOnPredicate,
  prettyGraphPrinter
) where

import Control.Exception (assert)

import Data.List (nub, intercalate)
import Data.Map.Strict as Map
import Data.Maybe
import Data.Set as Set

import Atom hiding (Atom)
import qualified Atom as At
import Pretty

data NodeData v u n = NodeData {
  unfolding :: u,
  predecessors :: Set n,
  outputVariables :: Set v
}

data Graph v u n = Graph {dataMap :: (Map n (NodeData v u n))}

-- basic accessing functions

domain :: Graph v u n -> Set n
domain (Graph m) = keysSet m

dataOfNode :: Ord n => Graph v u n -> n -> NodeData v u n
dataOfNode (Graph m) node =
  let err = error "node not in Graph"
  in fromMaybe err (Map.lookup node m)

unfoldNode :: Ord n => Graph v u n -> n -> u
unfoldNode graph node = unfolding (dataOfNode graph node)

outputVariablesOfNode :: Ord n => Graph v u n -> n -> Set v
outputVariablesOfNode graph node = outputVariables (dataOfNode graph node)

successorsOfNode :: Ord n => (u -> Set n) -> Graph v u n -> n -> Set n
successorsOfNode nodesInUnfolding graph node =
  nodesInUnfolding (unfoldNode graph node)

predecessorsOfNode :: Ord n => Graph v u n -> n -> Set n
predecessorsOfNode graph node = predecessors (dataOfNode graph node)

-- checking the consistency of the data

succPredInDom :: Ord n => (u -> Set n) -> Graph v u n -> Bool
succPredInDom nodesInUnfolding graph =
 all ((`isSubsetOf` dom) . succs) dom &&
 all ((`isSubsetOf` dom) . preds) dom where
    succs = successorsOfNode nodesInUnfolding graph
    preds = predecessorsOfNode graph
    dom = domain graph

converse :: Ord y => Set x -> (x -> Set y) -> (y -> Set x)
converse dom fct v = Set.filter (\u -> v `Set.member` fct u) dom

sameOnDom :: (Ord x, Eq y) => Set x -> (x -> y) -> (x -> y) -> Bool
sameOnDom dom f g = all (\a -> f a == g a) dom

succPredMatch :: Ord n => (u -> Set n) -> Graph v u n -> Bool
succPredMatch nodesInUnfolding graph =
  (sameOnDom dom (predecessorsOfNode graph) pred') where
    dom = domain graph
    pred' = converse dom (successorsOfNode nodesInUnfolding graph)

inputVariablesFromSuccessors :: (Ord v, Ord n) => (n -> Set v) -> Set n
                                                  -> Maybe (Set v)
inputVariablesFromSuccessors outVars succsSet =
  fmap outVars (Set.lookupMin succsSet)

outputVariablesCoherent :: (Ord v, Ord n) =>
                           (u -> Set n) -> (u -> Set v -> Set v)
                           -> Graph v u n -> Bool
outputVariablesCoherent nodesInUnfolding outputVariablesFromInputVariables
                        graph =
    all checkVariablesAroundNode (Map.toList (dataMap graph)) where
  checkVariablesAroundNode (_, NodeData uf _ outVars) = let
      maybeInputVars = let succsSet = nodesInUnfolding uf
                           outVars = outputVariablesOfNode graph
                           someInVars = inputVariablesFromSuccessors outVars
                                                                     succsSet
                           allInVars = Prelude.map outVars (Set.toList succsSet)
        in case someInVars of
             Just set -> if (all (== set) allInVars)
                           then Just set
                           else Nothing
             Nothing -> Just (error "we don't have incoming nodes")
                          -- this error will only trigger if there is a
                          -- mistake in outputVariablesFromInputVariables
    in case maybeInputVars of
         Just set -> outputVariablesFromInputVariables uf set == outVars
         Nothing  -> False

-- Need to make sure that this coherency test is applied as an assert in all
-- creation functions using this module. This is not done in the creation
-- functions defined here because then their signature would depend on
-- whether assertions are activated.
isCoherent :: (Ord v, Ord n) => (u -> Set n) -> (u -> Set v -> Set v)
                                -> Graph v u n -> Bool
isCoherent nodesInUnfolding outputVariablesFromInputVariables graph =
  succPredInDom nodesInUnfolding graph &&
  succPredMatch nodesInUnfolding graph &&
  outputVariablesCoherent nodesInUnfolding outputVariablesFromInputVariables graph

-- creation

isNub :: Eq a => [a] -> Bool
isNub list = length list == length (nub list)

-- This function would have room for optimization
fromTupleList :: (Ord v, Ord n) => (u -> Set n) -> [(n, u, Set v)]
                                   -> Graph v u n
fromTupleList nodesInUnfolding tupleList =
  assert (isNub (Prelude.map (\(f,_,_) -> f) tupleList)) $
    let domSet = Set.fromList (Prelude.map (\(f,_,_) -> f) tupleList)
        ufList = Prelude.map (\(f,uf,_) -> (f,uf)) tupleList
        succs n = nodesInUnfolding (fromJust (Prelude.lookup n ufList))
        preds = converse domSet succs
        aList = Prelude.map (\(n,u,v) -> (n, NodeData u (preds n) v)) tupleList
    in Graph (Map.fromList aList)

fromSet :: (Ord v, Ord n) => (u -> Set n) -> Set n -> (n -> (u, Set v))                                      -> Graph v u n
fromSet nodesInUnfolding domSet f = let
    succs = nodesInUnfolding . fst . f
    preds = converse domSet succs
  in Graph (Map.fromSet (\n -> let (uf, ov) = f n in NodeData uf (preds n) ov) domSet)

fromMap :: (Ord v, Ord n) => (u -> Set n) -> Map n (u, Set v) -> Graph v u n
fromMap nodesInUnfolding m = let
    domSet = Map.keysSet m
    succs n = nodesInUnfolding . fst $ m ! n
    preds = converse domSet succs
  in Graph (mapWithKey (\n (u, s) -> NodeData u (preds n) s) m)

subgraphOnSubset :: (Ord v, Ord n) => (u -> u) -> Set n -> Graph v u n
                                      -> Graph v u n
subgraphOnSubset filterUnfolding subset graph = let
    f node = let
          NodeData ouf opreds ov  = dataOfNode graph node
          uf = filterUnfolding ouf
          preds = opreds `Set.intersection` subset
        in NodeData uf preds ov
    subgraph = Graph (Map.fromSet f subset)
  in assert (subset `isSubsetOf` keysSet (dataMap graph)) subgraph

subgraphOnPredicate :: (Ord v, Ord n) => (u -> u) -> (n -> Bool)
                                         -> Graph v u n -> Graph v u n
subgraphOnPredicate filterUnfolding inSubgraph (Graph gm) = let
    f node inData = let
          NodeData ouf opreds ov  = inData
          uf = filterUnfolding ouf
          preds = Set.filter inSubgraph opreds
        in if inSubgraph node
             then Just (NodeData uf preds ov)
             else Nothing
  in Graph (Map.mapMaybeWithKey f gm)

-- pretty printing

prettyGraphPrinter :: (u -> String) -> (v -> String) -> (n -> String)
                      -> Graph v u n -> String
prettyGraphPrinter prettyUnfolding pVar pNode (Graph m) =
  let prettyOutVars set = intercalate ", " (Prelude.map pVar (Set.toList set))
      prettyNode (n, NodeData uf _ outVars) =
        (pNode n) ++ " : [" ++ prettyOutVars outVars ++ "] > "
                  ++ prettyUnfolding uf ++ "\n"
  in concatMap prettyNode (Map.toList m)
