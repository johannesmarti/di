module TermGraph (
  UnfoldingTerm,
  Graph,
  fromLogicalGraph,
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

nodesInUnfoldingTerm :: Ord n => UnfoldingTerm r v n -> Set n
nodesInUnfoldingTerm (Layer uf) =
  LG.foldUnfolding $ LG.mapUnfolding nodesInUnfoldingTerm uf
nodesInUnfoldingTerm (Leaf n) = Set.singleton n

type Graph r v n = BG.Graph v (UnfoldingTerm r v n) n

-- for checking the consistency of the data

isCoherent :: (Ord v, Ord n) => Graph r v n -> Bool
isCoherent = BG.isCoherent nodesInUnfoldingTerm undefined

-- These are some constructors from LogicalGraph. They should maybe go into their own file

-- This could already gather the successors. Then one would not have to
-- combine with a seperate call to nodesInUnfoldingTerm in many
-- applications.
unfoldNodeInGraph :: (Ord r, Ord v, Ord n) =>
  Set n -> LG.Graph r v n -> n -> UnfoldingTerm r v n
unfoldNodeInGraph anchors graph node =
  if node `member` anchors
    then Leaf node
    else Layer $ LG.mapUnfolding (unfoldNodeInGraph anchors graph)
                                 (LG.unfoldNode graph node)

unfoldPastNodeInGraph :: (Ord r, Ord v, Ord n) =>
  Set n -> LG.Graph r v n -> n -> UnfoldingTerm r v n
unfoldPastNodeInGraph anchors graph node =
    Layer $ LG.mapUnfolding (unfoldNodeInGraph anchors graph)
                            (LG.unfoldNode graph node)

unfoldAtSet :: (Ord r, Ord v, Ord n) => Set n -> LG.Graph r v n -> Graph r v n
unfoldAtSet anchors graph = assert (isCoherent result) result where
  result = BG.fromSet nodesInUnfoldingTerm anchors f
  f node = (unfoldPastNodeInGraph anchors graph node,
            BG.outputVariablesOfNode graph node)

fromLogicalGraph :: (Ord r, Ord v, Ord n) =>
  [n] -> LG.Graph r v n -> Graph r v n
fromLogicalGraph roots graph = unfoldAtSet anchors graph where
  anchors = Set.fromList roots `Set.union` multipleIncoming
  multipleIncoming = Set.filter hasMultipleIncoming $ LG.domain graph
  hasMultipleIncoming node = size (LG.predecessorsOfNode graph node) > 1

-- pretty printing

prettyUnfoldingTerm :: LG.PrettyLogicalBase r v n ->
  UnfoldingTerm r v n -> String
prettyUnfoldingTerm pb (Layer uf) = "(" ++ LG.prettyUnfolding pb' uf ++ ")" where
  pb' = pb { LG.prettyNode = prettyUnfoldingTerm pb }
prettyUnfoldingTerm pb (Leaf n) = LG.prettyNode pb n

prettyGraphPrinter :: LG.PrettyLogicalBase r v n -> Graph r v n -> String
prettyGraphPrinter pb = BG.prettyGraphPrinter (prettyUnfoldingTerm pb)
                                              (LG.prettyVariable pb)
                                              (LG.prettyNode pb)

showGraph :: (Show r, Show v, Show n) => Graph r v n -> String
showGraph = prettyGraphPrinter $ LG.PrettyLogicalBase show show show

prettyGraph :: (Pretty r, Pretty v, Pretty n) => Graph r v n -> String
prettyGraph = prettyGraphPrinter $ LG.PrettyLogicalBase pretty pretty pretty

