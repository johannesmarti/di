module Main (main) where

import Test.Hspec

import qualified TestOptimizer
import qualified TestTermGraph
import qualified TestLeapFrog

main :: IO ()
main = hspec $ do
  describe "Optimizer tests" TestOptimizer.spec
  describe "TermGraph tests" TestTermGraph.spec
  describe "LeapFrog tests" TestLeapFrog.spec
