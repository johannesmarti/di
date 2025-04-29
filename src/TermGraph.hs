module TermGraph (
  Term,
  Graph,
  fromLogicalGraph,
  prettyGraph
) where

import Control.Exception (assert)

import Data.Set as Set

import qualified BaseGraph as BG
import qualified OperatorGraph as OG
import Pretty


data Term r v n =
    Layer (OG.Operator r v (Term r v n)) | Leaf n
  deriving (Eq,Ord)

nodesInTerm :: Ord n => Term r v n -> Set n
nodesInTerm (Layer uf) =
  OG.foldOperator $ OG.mapOperator nodesInTerm uf
nodesInTerm (Leaf n) = Set.singleton n

type Graph r v n = BG.Graph v (Term r v n) n

-- for checking the consistency of the data

isCoherent :: (Ord v, Ord n) => Graph r v n -> Bool
isCoherent = BG.isCoherent nodesInTerm undefined

-- These are some constructors from LogicalGraph. They should maybe go into their own file

-- This could already gather the successors. Then one would not have to
-- combine with a seperate call to nodesInTerm in many
-- applications.
unfoldNodeInGraph :: (Ord r, Ord v, Ord n) =>
  Set n -> OG.Graph r v n -> n -> Term r v n
unfoldNodeInGraph anchors graph node =
  if node `member` anchors
    then Leaf node
    else Layer $ OG.mapOperator (unfoldNodeInGraph anchors graph)
                                 (OG.unfoldNode graph node)

unfoldPastNodeInGraph :: (Ord r, Ord v, Ord n) =>
  Set n -> OG.Graph r v n -> n -> Term r v n
unfoldPastNodeInGraph anchors graph node =
    Layer $ OG.mapOperator (unfoldNodeInGraph anchors graph)
                            (OG.unfoldNode graph node)

unfoldAtSet :: (Ord r, Ord v, Ord n) => Set n -> OG.Graph r v n -> Graph r v n
unfoldAtSet anchors graph = assert (isCoherent result) result where
  result = BG.fromSet nodesInTerm anchors f
  f node = (unfoldPastNodeInGraph anchors graph node,
            BG.outputVariablesOfNode graph node)

fromLogicalGraph :: (Ord r, Ord v, Ord n) => [n] -> OG.Graph r v n -> Graph r v n
fromLogicalGraph roots graph = unfoldAtSet anchors graph where
  anchors = Set.fromList roots `Set.union` multipleIncoming
  multipleIncoming = Set.filter hasMultipleIncoming $ OG.domain graph
  hasMultipleIncoming node = size (OG.predecessorsOfNode graph node) > 1

-- pretty printing

prettyTerm :: OG.PrettyOperatorBase r v n -> Term r v n -> String
prettyTerm pb (Layer uf) = "(" ++ OG.prettyOperator pb' uf ++ ")" where
  pb' = pb { OG.prettyNode = prettyTerm pb }
prettyTerm pb (Leaf n) = OG.prettyNode pb n

prettyGraphPrinter :: OG.PrettyOperatorBase r v n -> Graph r v n -> String
prettyGraphPrinter pb = BG.prettyGraphPrinter (prettyTerm pb)
                                              (OG.prettyVariable pb)
                                              (OG.prettyNode pb)

showGraph :: (Show r, Show v, Show n) => Graph r v n -> String
showGraph = prettyGraphPrinter $ OG.PrettyOperatorBase show show show

prettyGraph :: (Pretty r, Pretty v, Pretty n) => Graph r v n -> String
prettyGraph = prettyGraphPrinter $ OG.PrettyOperatorBase pretty pretty pretty

