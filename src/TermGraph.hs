module TermGraph (
  UnfoldingTerm,
  Graph,
  unfoldAtSet,
  prettyGraph
) where

import Control.Exception (assert)

import Data.Set as Set

import qualified BaseGraph as BG
import qualified LogicalGraph as LG
import Pretty


data UnfoldingTerm r v n =
  Layer (LG.Unfolding r v (UnfoldingTerm r v n)) | Leaf n
  deriving (Eq,Ord)

type Graph r v n = BG.Graph v (UnfoldingTerm r v n) n

-- checking the consistency of the data

unfoldNodeInGraph :: (Ord r, Ord v, Ord n) =>
  Set n -> LG.Graph r v n -> n -> UnfoldingTerm r v n
unfoldNodeInGraph anchors graph node =
  if node `member` anchors
    then Leaf node
    else Layer $ LG.mapUnfolding (unfoldNodeInGraph anchors graph)
                                 (LG.unfoldNode graph node)

unfoldAtSet :: Set n -> LG.Graph r v n -> Graph r v n
unfoldAtSet = undefined

-- pretty printing

prettyUnfoldingTerm :: LG.PrettyLogicalBase r v n ->
  UnfoldingTerm r v n -> String
prettyUnfoldingTerm pb (Layer uf) = LG.prettyUnfolding pb' uf where
  pb' = pb { LG.prettyNode = prettyUnfoldingTerm pb }
prettyUnfoldingTerm pb (Leaf n) = LG.prettyNode pb n

prettyGraphPrinter :: LG.PrettyLogicalBase r v n -> Graph r v n -> String
prettyGraphPrinter pb = BG.prettyGraphPrinter (prettyUnfoldingTerm pb)
                                              (LG.prettyVariable pb)
                                              (LG.prettyNode pb)

showGraph :: (Show r, Show v, Show n) => Graph r v n -> String
showGraph = prettyGraphPrinter $ LG.PrettyLogicalBase show show show

prettyGraph :: (Pretty r, Pretty v, Pretty n) =>
                      Graph r v n -> String
prettyGraph = prettyGraphPrinter $ LG.PrettyLogicalBase pretty pretty pretty

