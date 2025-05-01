module TestOptimizer (spec) where

import Test.Hspec

import Examples.OperatorGraphs
import OperatorGraph
import Optimizer.RemoveTopBot
import Optimizer.Reachability

spec :: Spec
spec = describe "example" $ do

  let optimizedTransitive = removeTop . removeBot $ transitiveGraph
  it "no change on remove top and bot from transitive" $
    domain transitiveGraph `shouldBe` domain optimizedTransitive

  let reachableTransitive = keepReachable [2] transitiveGraph
  it "no change on keepReachable for transitive" $
    domain transitiveGraph `shouldBe` domain reachableTransitive
