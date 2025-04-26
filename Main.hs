module Main where

import Examples.LogicalGraphs
import Examples.Programs
import Data.Map

import LogicalGraph
import Optimizer.RemoveTopBot
import Optimizer.Reachability
import ProgramToGraph

main :: IO ()
main = do
  --putStrLn $ prettyGraph trivialGraph
  --putStrLn "==========="
  putStrLn $ prettyGraph transitiveGraph
  putStrLn "==========="
  putStrLn $ prettyGraph (keepReachable [2] . removeTop . removeBot $ transitiveGraph)
  --putStrLn "==========="
  --putStrLn . prettyGraph $ programToGraph
  --          (fromList [('T', 2)], fromList [('R', 2)]) transitive
  --putStrLn "==========="
  --putStrLn . prettyGraph $ programToGraph
  --          (fromList [('U', 1),('F', 2)],
  --           fromList [('B',1),('M', 2),('P',3)])
  --          manages
