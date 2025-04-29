module TestTermGraph (spec) where

import Test.Hspec

import Data.Set

import Examples.LogicalGraphs
import qualified BaseGraph as BG
import qualified TermGraph as TG

spec :: Spec
spec = describe "example" $ do

  let termGraph = TG.fromLogicalGraph [2] transitiveGraph
  let domainTermGraph = BG.domain termGraph
  it "transitive graph is reduced to two states" $
    size domainTermGraph `shouldBe` 2
