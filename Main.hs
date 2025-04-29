module Main where

import Data.Map

import Examples.OperatorGraphs
import Examples.Programs
import OperatorGraph as OG
import TermGraph as TG
import Optimizer.RemoveTopBot
import Optimizer.Reachability
import ProgramToGraph

main :: IO ()
main = do
  --putStrLn $ prettyGraph trivialGraph
  --putStrLn "==========="
  putStrLn $ OG.prettyGraph transitiveGraph
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
