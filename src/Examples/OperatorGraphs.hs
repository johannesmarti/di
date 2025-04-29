module Examples.OperatorGraphs (
  trivialGraph,
  transitiveGraph,
) where

import Data.Set as Set

import qualified Atom 
import OperatorGraph

fl :: (Ord v, Ord n) => [(n, Operator r v n, [v])] -> Graph r v n
fl = fromTupleList . (Prelude.map (\(a,b,c) -> (a,b, Set.fromList c)))

trivial :: [(Int, Operator Char Char Int, [Char])]
trivial = [
  (1, Atom (Atom.Atom 'R' ['x', 'y']), ['x', 'y']) ]

trivialGraph :: Graph Char Char Int
trivialGraph = fl trivial

transitive :: [(Int, Operator Char Char Int, [Char])]
transitive = [
  (1, OperatorGraph.Atom (Atom.Atom 'R' ['x', 'y']), ['x', 'y']),

  (2, Or (Set.fromList [1, 3]), ['x', 'y']),
  (3, Exists 'z' 4, ['x', 'y']),

  (4, And (Set.fromList [5, 6]), ['x', 'z', 'y']),

  (5, Project 'y' 7, ['x', 'z', 'y']),
  (7, Assign 'z' 'y' 2, ['x', 'z']),

  (6, Project 'x' 8, ['x', 'z', 'y']),
  (8, Assign 'z' 'x' 1, ['z', 'y']) ]

transitiveGraph :: Graph Char Char Int
transitiveGraph = fl transitive
