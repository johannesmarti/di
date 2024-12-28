module Main where

import Examples.Graphs
import Examples.Programs

import Graph

main :: IO ()
main = do
  putStrLn $ prettyGraph trivialGraph
  putStrLn $ prettyGraph transitiveGraph
