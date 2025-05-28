module Main (main) where

import Test.Hspec

import qualified OptimizerTest
import qualified TermGraphTest
import qualified LeapFrogTest

main :: IO ()
main = hspec $ do
  describe "Optimizer tests" OptimizerTest.spec
  describe "TermGraph tests" TermGraphTest.spec
  describe "LeapFrog tests" LeapFrogTest.spec
