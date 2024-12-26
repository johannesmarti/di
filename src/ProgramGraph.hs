module ProgramGraph (

) where

import Control.Exception (assert)

import Data.Set as Set
import Data.Map.Strict as Map
import Data.Maybe

import qualified DatalogProgram as DP

data Unfolding r v n = Atom (DP.Atom r v) | Equality v v | Top | Bot |
                       And n n | Or n n | Exists v n | Project v n |
                       Assign v v n

data ProgramGraph r v n = ProgramGraph {
  unfolding :: Map n (Unfolding r v n),
  predecessorMap :: Map n (Set n),
  outputVariablesMap :: Map n (Set v)
}


-- basic accessing functions

domain :: ProgramGraph r v n -> Set n
domain graph = keysSet (unfolding graph)

outputVariables :: Ord n => ProgramGraph r v n -> n -> Set v
outputVariables graph node = 
  let err = error "node not in ProgramGraph"
  in fromMaybe err (Map.lookup node (outputVariablesMap graph))

unfold :: Ord n => ProgramGraph r v n -> n -> Unfolding r v n
unfold graph node =
  let err = error "node not in ProgramGraph"
  in fromMaybe err (Map.lookup node (unfolding graph))

nodesInUnfolding :: Ord n => Unfolding r v n -> Set n
nodesInUnfolding (And a b) = Set.fromList [a,b]
nodesInUnfolding (Or a b) = Set.fromList [a,b]
nodesInUnfolding (Exists v a) = Set.singleton a
nodesInUnfolding (Project v a) = Set.singleton a
nodesInUnfolding (Assign v w a) = Set.singleton a
nodesInUnfolding _ = Set.empty

successors :: Ord n => ProgramGraph r v n -> n -> Set n
successors graph node = nodesInUnfolding (unfold graph node)

predecessors :: Ord n => ProgramGraph r v n -> n -> Set n
predecessors graph node =
  let err = error "node not in ProgramGraph"
  in fromMaybe err (Map.lookup node (predecessorMap graph))


-- checking the consistency of the data

domsAreCoherent :: Ord n => ProgramGraph r v n -> Bool
domsAreCoherent graph =
  dom == keysSet (predecessorMap graph) && 
  dom == keysSet (outputVariablesMap graph) where dom = domain graph

succPredInDom :: Ord n => ProgramGraph r v n -> Bool
succPredInDom graph =
 all ((`isSubsetOf` dom) . succ) dom &&
 all ((`isSubsetOf` dom) . pred) dom where
    succ = successors graph
    pred = predecessors graph
    dom = domain graph

converse :: Ord y => Set x -> (x -> Set y) -> (y -> Set x)
converse dom fct v = Set.filter (\u -> v `Set.member` fct u) dom

sameOnDom :: (Ord x, Eq y) => Set x -> (x -> y) -> (x -> y) -> Bool
sameOnDom dom f g = all (\a -> f a == g a) dom

succPredMatch :: Ord n => ProgramGraph r v n -> Bool
succPredMatch graph = (sameOnDom dom (predecessors graph) pred') where
  dom = domain graph
  pred' = converse dom (successors graph)

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
    all checkVariablesAroundNode (Map.toList (unfolding graph)) where
  checkVariablesAroundNode (n, uf) = let
      maybeInputVars = let succsSet = successors graph n
                           someNode = Set.lookupMin succsSet
                           someInVars = fmap (outputVariables graph) someNode 
                           allInVars = Prelude.map (outputVariables graph)
                                                   (Set.toList succsSet)
        in case someInVars of
             Just set -> if (all (== set) allInVars)
                           then Just set
                           else Nothing
             Nothing -> error "we don't have incoming nodes"
                          -- this error will only trigger if there is a
                          -- mistake in outputVariablesFromInputVariables
      outVars = outputVariables graph n
    in case maybeInputVars of
         Just set -> outputVariablesFromInputVariables uf set == outVars
         Nothing  -> False

isCoherent :: (Ord v, Ord n) => ProgramGraph r v n -> Bool
isCoherent graph =
  domsAreCoherent graph &&
  succPredInDom graph &&
  succPredMatch graph &&
  outputVariablesCoherent graph

-- construction
