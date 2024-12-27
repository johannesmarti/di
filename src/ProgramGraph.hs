module ProgramGraph (
  ProgramGraph,
  fromTupleList
) where

import Control.Exception (assert)

import Data.List (nub)
import Data.Map.Strict as Map
import Data.Maybe
import Data.Set as Set

import qualified DatalogProgram as DP


data Unfolding r v n = Atom (DP.Atom r v) | Equality v v | Top | Bot |
                       And n n | Or n n | Exists v n | Project v n |
                       Assign v v n

data NodeData r v n = NodeData {
  unfolding :: Unfolding r v n,
  predecessors :: Set n,
  outputVariables :: Set v
}

data ProgramGraph r v n = ProgramGraph {dataMap :: (Map n (NodeData r v n))}

-- basic accessing functions

domain :: ProgramGraph r v n -> Set n
domain (ProgramGraph m) = keysSet m

dataOfNode :: Ord n => ProgramGraph r v n -> n -> NodeData r v n
dataOfNode (ProgramGraph m) node =
  let err = error "node not in ProgramGraph"
  in fromMaybe err (Map.lookup node m)

unfoldNode :: Ord n => ProgramGraph r v n -> n -> Unfolding r v n
unfoldNode graph node = unfolding (dataOfNode graph node)

outputVariablesOfNode :: Ord n => ProgramGraph r v n -> n -> Set v
outputVariablesOfNode graph node = outputVariables (dataOfNode graph node)

nodesInUnfolding :: Ord n => Unfolding r v n -> Set n
nodesInUnfolding (And a b) = Set.fromList [a,b]
nodesInUnfolding (Or a b) = Set.fromList [a,b]
nodesInUnfolding (Exists v a) = Set.singleton a
nodesInUnfolding (Project v a) = Set.singleton a
nodesInUnfolding (Assign v w a) = Set.singleton a
nodesInUnfolding _ = Set.empty

successorsOfNode :: Ord n => ProgramGraph r v n -> n -> Set n
successorsOfNode graph node = nodesInUnfolding (unfoldNode graph node)

predecessorsOfNode :: Ord n => ProgramGraph r v n -> n -> Set n
predecessorsOfNode graph node = predecessors (dataOfNode graph node)


-- checking the consistency of the data

succPredInDom :: Ord n => ProgramGraph r v n -> Bool
succPredInDom graph =
 all ((`isSubsetOf` dom) . succs) dom &&
 all ((`isSubsetOf` dom) . preds) dom where
    succs = successorsOfNode graph
    preds = predecessorsOfNode graph
    dom = domain graph

converse :: Ord y => Set x -> (x -> Set y) -> (y -> Set x)
converse dom fct v = Set.filter (\u -> v `Set.member` fct u) dom

sameOnDom :: (Ord x, Eq y) => Set x -> (x -> y) -> (x -> y) -> Bool
sameOnDom dom f g = all (\a -> f a == g a) dom

succPredMatch :: Ord n => ProgramGraph r v n -> Bool
succPredMatch graph = (sameOnDom dom (predecessorsOfNode graph) pred') where
  dom = domain graph
  pred' = converse dom (successorsOfNode graph)

outputVariablesFromInputVariables :: Ord v => Unfolding r v n -> Set v -> Set v
outputVariablesFromInputVariables (Atom at) _ = Set.fromList $ DP.arguments at
outputVariablesFromInputVariables (Equality v w) _ = Set.fromList $ [v, w]
outputVariablesFromInputVariables Top _ = Set.empty
outputVariablesFromInputVariables Bot _ = Set.empty
outputVariablesFromInputVariables (And _ _) inVars = inVars
outputVariablesFromInputVariables (Or _ _) inVars = inVars
outputVariablesFromInputVariables (Exists v _) inVars = Set.delete v inVars
outputVariablesFromInputVariables (Project v _) inVars =
  assert (not $ v `Set.member` inVars) $ Set.insert v inVars
outputVariablesFromInputVariables (Assign v w _) inVars = 
  assert (not $ v `Set.member` inVars) $ Set.insert v (Set.delete w inVars)
-- TODO: in pretty code these asserts where also checked as part of the bool computation

outputVariablesCoherent :: (Ord v, Ord n) => ProgramGraph r v n -> Bool
outputVariablesCoherent graph =
    all checkVariablesAroundNode (Map.toList (dataMap graph)) where
  checkVariablesAroundNode (_, NodeData uf _ outVars) = let
      maybeInputVars = let succsSet = nodesInUnfolding uf
                           someNode = Set.lookupMin succsSet
                           someInVars = fmap (outputVariablesOfNode graph)
                                             someNode 
                           allInVars = Prelude.map (outputVariablesOfNode graph)
                                                   (Set.toList succsSet)
        in case someInVars of
             Just set -> if (all (== set) allInVars)
                           then Just set
                           else Nothing
             Nothing -> error "we don't have incoming nodes"
                          -- this error will only trigger if there is a
                          -- mistake in outputVariablesFromInputVariables
    in case maybeInputVars of
         Just set -> outputVariablesFromInputVariables uf set == outVars
         Nothing  -> False

-- Should also check that renamed and bound and projected variables make
-- sense. I am not yet sure how to do this well.
isCoherent :: (Ord v, Ord n) => ProgramGraph r v n -> Bool
isCoherent graph =
  succPredInDom graph &&
  succPredMatch graph &&
  outputVariablesCoherent graph

-- creation
isNub :: Eq a => [a] -> Bool
isNub list = length list == length (nub list)

fromTupleList :: (Ord v, Ord n) => [(n, Unfolding r v n, Set v)]
                                   -> ProgramGraph r v n
fromTupleList tupleList = assert (isNub (Prelude.map (\(f,_,_) -> f) tupleList)) $
  let domSet = Set.fromList (Prelude.map (\(f,_,_) -> f) tupleList)
      ufList = Prelude.map (\(f,uf,_) -> (f,uf)) tupleList
      succs n = nodesInUnfolding (fromJust (error "your mum")
                                          (Prelude.lookup n ufList))
      preds = converse domSet succs
      aList = Prelude.map (\(n,u,v) -> (n, NodeData u (preds n) v)) tupleList
      graph = ProgramGraph (Map.fromList aList)
  in assert (isCoherent graph) graph

