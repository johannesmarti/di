module OperatorGraph (
  Operator(..),
  mapOperator,
  foldOperator,
  BG.unfolding,
  outputVariablesFromInputVariables,
  outputVariablesOfOperator,
  Graph,
  BG.dataMap,
  BG.domain,
  BG.unfoldNode,
  successorsOfNode,
  BG.predecessorsOfNode,
  fromTupleList,
  OperatorGraph.fromSet,
  fromMap,
  subgraphOnSubset,
  subgraphOnPredicate,
  PrettyOperatorBase(..),
  prettyOperator,
  prettyGraph
) where

import Control.Exception (assert)

import Data.Foldable (fold)
import Data.List (nub, intercalate)
import Data.Map.Strict as Map
import Data.Maybe
import Data.Set as Set

import Atom hiding (Atom)
import qualified Atom as At
import qualified BaseGraph as BG
import Pretty

data Switch v = Switch {
  outSet :: Set v,
  inSet :: Set v
} deriving (Eq, Ord)

data Operator r v n = Atom (At.Atom r v) |
                      And (Set n) | Or (Set n) |
                      VariableSwitch (Switch v) n
  deriving (Eq, Ord)

mapOperator :: Ord m => (n -> m) -> Operator r v n -> Operator r v m
mapOperator f (Atom a) = Atom a
mapOperator f (And s) = And (Set.map f s)
mapOperator f (Or s) = Or (Set.map f s)
mapOperator f (VariableSwitch s n) = VariableSwitch s (f n)

foldOperator :: Monoid n => Operator r v n -> n
foldOperator (Atom a) = mempty
foldOperator (And s) = Data.Foldable.fold s
foldOperator (Or s) = Data.Foldable.fold s 
foldOperator (VariableSwitch _ n) = n

nodesInOperator :: Ord n => Operator r v n -> Set n
nodesInOperator (Atom _) = Set.empty
nodesInOperator (And s) = s
nodesInOperator (Or s) = s
nodesInOperator (VariableSwitch _ a) = Set.singleton a

outputVariablesFromSet :: Ord v => Set (Set v) -> Set v
outputVariablesFromSet setOfSets = assert (Set.size setOfSets <= 1) $
  case Set.lookupMin setOfSets of
    Nothing -> Set.empty
    Just s -> s

outputVariablesFromInputVariables :: Ord v => Operator r v (Set v) -> Set v
outputVariablesFromInputVariables (Atom at) = Set.fromList $ arguments at
outputVariablesFromInputVariables (And inVarSet) =
  outputVariablesFromSet inVarSet
outputVariablesFromInputVariables (Or inVarSet) =
  outputVariablesFromSet inVarSet
outputVariablesFromInputVariables (VariableSwitch (Switch ov iv) inVars) =
  assert (ov `Set.disjoint` inVars) $
  assert (iv `Set.isSubsetOf` inVars) $ Set.union ov (Set.difference inVars iv)

outputVariablesOfOperator :: Ord v => (n -> Set v) -> Operator r v n -> Set v
outputVariablesOfOperator outputVariablesOfNode =
  outputVariablesFromInputVariables . mapOperator outputVariablesOfNode

type Graph r v n = BG.Graph v (Operator r v n) n

successorsOfNode :: Ord n => Graph r v n -> n -> Set n
successorsOfNode = BG.successorsOfNode nodesInOperator

-- checking the consistency of the data

-- TODO: Should also check that renamed, bound and projected variables make
-- sense. I am not yet sure how to do this well.
isCoherent :: (Ord v, Ord n) => Graph r v n -> Bool
isCoherent = BG.isCoherent outputVariablesOfOperator nodesInOperator

-- creation

fromTupleList :: (Ord v, Ord n) => [(n, Operator r v n, Set v)]
                                   -> Graph r v n
fromTupleList tupleList = assert (isCoherent result) result where
  result = BG.fromTupleList nodesInOperator tupleList

fromSet :: (Ord v, Ord n) => Set n -> (n -> (Operator r v n, Set v))
                             -> Graph r v n
fromSet domSet f = assert (isCoherent result) result where
  result = BG.fromSet nodesInOperator domSet f

fromMap :: (Ord v, Ord n) => Map n (Operator r v n, Set v) -> Graph r v n
fromMap m = assert (isCoherent result) result where
  result = BG.fromMap nodesInOperator m

subgraphOnSubset :: (Ord v, Ord n) => Set n -> Graph r v n -> Graph r v n
subgraphOnSubset subset graph = assert (isCoherent result) result where
  result = BG.subgraphOnSubset filterOperator subset graph
  filterOperator uf = case uf of
                         And nodes -> And (nodes `Set.intersection` subset)
                         Or  nodes -> Or  (nodes `Set.intersection` subset)
                         _         -> uf

subgraphOnPredicate :: (Ord v, Ord n) => (n -> Bool) -> Graph r v n
                                         -> Graph r v n
subgraphOnPredicate inSubgraph graph =
  assert (isCoherent result) result where
    result = BG.subgraphOnPredicate filterOperator inSubgraph graph
    filterOperator uf = case uf of
                           And nodes -> And (Set.filter inSubgraph nodes)
                           Or  nodes -> Or  (Set.filter inSubgraph nodes)
                           _         -> uf


-- pretty printing

prettySet :: (e -> String) -> Set e -> String
prettySet prettyElement set =
  "{" ++ intercalate ", " (Prelude.map prettyElement (Set.toList set)) ++ "}"

data PrettyOperatorBase r v n = PrettyOperatorBase {
  prettyRelation :: r -> String,
  prettyVariable :: v -> String,
  prettyNode :: n -> String
}

prettyOperator :: PrettyOperatorBase r v n -> Operator r v n -> String
prettyOperator pb (Atom dpAtom) =
  prettyAtom (prettyRelation pb) (prettyVariable pb) dpAtom
prettyOperator pb (And s) = "and " ++ prettySet (prettyNode pb) s
prettyOperator pb (Or s) = "or " ++ prettySet (prettyNode pb) s
prettyOperator pb (VariableSwitch (Switch ov iv) x) = let
    pVars s = intercalate ", " (Prelude.map (prettyVariable pb) (Set.toList s))
  in "[" ++ pVars ov ++ "/" ++ pVars iv ++ "]" ++ prettyNode pb x

prettyGraphPrinter :: PrettyOperatorBase r v n -> Graph r v n -> String
prettyGraphPrinter pb = BG.prettyGraphPrinter (prettyOperator pb)
                                              (prettyVariable pb)
                                              (prettyNode pb)

showGraph :: (Show r, Show v, Show n) => Graph r v n -> String
showGraph = prettyGraphPrinter $ PrettyOperatorBase show show show

prettyGraph :: (Pretty r, Pretty v, Pretty n) => Graph r v n -> String
prettyGraph = prettyGraphPrinter $ PrettyOperatorBase pretty pretty pretty

