module Main where

import Data.Set as Set

import qualified Atom 
import ProgramGraph

trivial :: [(Int, Unfolding Char Char Int, [Char])]
trivial = [
  (1, Atom (Atom.Atom 'R' ['x', 'y']), ['x', 'y']) ]

transitive :: [(Int, Unfolding Char Char Int, [Char])]
transitive = [
  (1, ProgramGraph.Atom (Atom.Atom 'R' ['x', 'y']), ['x', 'y']),

  (2, Or 1 3, ['x', 'y']),
  (3, Exists 'z' 4, ['x', 'y']),

  (4, And 5 6, ['x', 'z', 'y']),

  (5, Project 'y' 7, ['x', 'z', 'y']),
  (7, Assign 'z' 'y' 2, ['x', 'z']),

  (6, Project 'x' 8, ['x', 'z', 'y']),
  (8, Assign 'z' 'x' 1, ['z', 'y']) ]

fl :: (Ord v, Ord n) => [(n, Unfolding r v n, [v])] -> ProgramGraph r v n
fl = fromTupleList . (Prelude.map (\(a,b,c) -> (a,b, Set.fromList c)))

main :: IO ()
main = let
    graphTrivial = fl trivial
    graphTransitive = fl transitive
  in do
    putStrLn $ prettyProgramGraph graphTrivial
    putStrLn $ prettyProgramGraph graphTransitive
