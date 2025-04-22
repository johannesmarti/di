module Main (main) where

import Test.Hspec

import qualified Optimizer

main :: IO ()
main = hspec $ do
  describe "Optimizer tests" Optimizer.spec
