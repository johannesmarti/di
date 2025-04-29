module Main where

import Data.Map

import Examples.LogicalGraphs
import Examples.Programs
import LogicalGraph as LG
import TermGraph as TG
import Optimizer.RemoveTopBot
import Optimizer.Reachability
import ProgramToGraph

main :: IO ()
main = do
  --putStrLn $ prettyGraph trivialGraph
  --putStrLn "==========="
  putStrLn $ LG.prettyGraph transitiveGraph
  putStrLn "==========="
  putStrLn $ TG.prettyGraph (TG.fromLogicalGraph [2] transitiveGraph)
  --putStrLn "==========="
  --putStrLn . prettyGraph $ programToGraph
  --          (fromList [('T', 2)], fromList [('R', 2)]) transitive
  --putStrLn "==========="
  --putStrLn . prettyGraph $ programToGraph
  --          (fromList [('U', 1),('F', 2)],
  --           fromList [('B',1),('M', 2),('P',3)])
  --          manages
