module Main where

import Examples.Graphs
import Examples.Programs
import Data.Map

import Graph
import ProgramToGraph

main :: IO ()
main = do
  putStrLn $ prettyGraph trivialGraph
  putStrLn "==========="
  putStrLn $ prettyGraph transitiveGraph
  putStrLn "==========="
  putStrLn . prettyGraph $ programToGraph
            (fromList [('T', 2)], fromList [('R', 2)]) transitive
  putStrLn "==========="
  putStrLn . prettyGraph $ programToGraph
            (fromList [('U', 1),('F', 2)],
             fromList [('B',1),('M', 2),('P',3)])
            manages
