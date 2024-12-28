module Examples.Graphs (
  trivialGraph,
  transitiveGraph,
) where

import Data.Set as Set

import qualified Atom 
import Graph

fl :: (Ord v, Ord n) => [(n, Unfolding r v n, [v])] -> Graph r v n
fl = fromTupleList . (Prelude.map (\(a,b,c) -> (a,b, Set.fromList c)))

trivial :: [(Int, Unfolding Char Char Int, [Char])]
trivial = [
  (1, Atom (Atom.Atom 'R' ['x', 'y']), ['x', 'y']) ]

trivialGraph :: Graph Char Char Int
trivialGraph = fl trivial

transitive :: [(Int, Unfolding Char Char Int, [Char])]
transitive = [
  (1, Graph.Atom (Atom.Atom 'R' ['x', 'y']), ['x', 'y']),

  (2, Or (Set.fromList [1, 3]), ['x', 'y']),
  (3, Exists 'z' 4, ['x', 'y']),

  (4, And (Set.fromList [5, 6]), ['x', 'z', 'y']),

  (5, Project 'y' 7, ['x', 'z', 'y']),
  (7, Assign 'z' 'y' 2, ['x', 'z']),

  (6, Project 'x' 8, ['x', 'z', 'y']),
  (8, Assign 'z' 'x' 1, ['z', 'y']) ]

transitiveGraph :: Graph Char Char Int
transitiveGraph = fl transitive
