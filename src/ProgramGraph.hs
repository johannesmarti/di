module ProgramGraph (

) where

import Data.Set as Set
import Data.Map.Strict as Map
import Data.Maybe

import DatalogProgram

data Unfolding r v n = Atom (Atom r v) | Equality v v | Top | Bot |
                       And n n | Or n n | Exists v n | Project v n |
                       Rename v v n

data ProgramGraph r v n = ProgramGraph {
  unfolding :: Map n (Unfolding r v n),
  predecessorMap :: Map n (Set n),
  outputVariablesMap :: Map n (Set v)
}

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
nodesInUnfolding (Rename v w a) = Set.singleton a
nodesInUnfolding _ = Set.empty

successors :: Ord n => ProgramGraph r v n -> n -> Set n
successors graph node = nodesInUnfolding (unfold graph node)

predecessors :: Ord n => ProgramGraph r v n -> n -> Set n
predecessors graph node =
  let err = error "node not in ProgramGraph"
  in fromMaybe err (Map.lookup node (predecessorMap graph))



