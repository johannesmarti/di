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

data Operator r v n = Atom (At.Atom r v) | Equality v v |
                      And (Set n) | Or (Set n) |
                      Exists v n | Project v n | Assign v v n
  deriving (Eq,Ord)

mapOperator :: Ord m => (n -> m) -> Operator r v n -> Operator r v m
mapOperator f (Atom a) = Atom a
mapOperator f (Equality v w) = Equality v w
mapOperator f (And s) = And (Set.map f s)
mapOperator f (Or s) = Or (Set.map f s)
mapOperator f (Exists v n) = Exists v (f n)
mapOperator f (Project v n) = Project v (f n)
mapOperator f (Assign v w n) = Assign v w (f n)

foldOperator :: Monoid n => Operator r v n -> n
foldOperator (Atom a) = mempty
foldOperator (Equality v w) = mempty
foldOperator (And s) = Data.Foldable.fold s
foldOperator (Or s) = Data.Foldable.fold s 
foldOperator (Exists v n) = n
foldOperator (Project v n) = n
foldOperator (Assign v w n) = n

nodesInOperator :: Ord n => Operator r v n -> Set n
nodesInOperator (And s) = s
nodesInOperator (Or s) = s
nodesInOperator (Exists v a) = Set.singleton a
nodesInOperator (Project v a) = Set.singleton a
nodesInOperator (Assign v w a) = Set.singleton a
nodesInOperator _ = Set.empty

outputVariablesFromSet :: Ord v => Set (Set v) -> Set v
outputVariablesFromSet setOfSets = assert (Set.size setOfSets <= 1) $
  case Set.lookupMin setOfSets of
    Nothing -> Set.empty
    Just s -> s

outputVariablesFromInputVariables :: Ord v => Operator r v (Set v) -> Set v
outputVariablesFromInputVariables (Atom at) = Set.fromList $ arguments at
outputVariablesFromInputVariables (Equality v w) = Set.fromList $ [v, w]
outputVariablesFromInputVariables (And inVars) = outputVariablesFromSet inVars
outputVariablesFromInputVariables (Or inVars) = outputVariablesFromSet inVars
outputVariablesFromInputVariables (Exists v inVars) = Set.delete v inVars
outputVariablesFromInputVariables (Project v inVars) =
  assert (not $ v `Set.member` inVars) $ Set.insert v inVars
outputVariablesFromInputVariables (Assign v w inVars) = 
  assert (not $ v `Set.member` inVars)
  assert (w `Set.member` inVars) $ Set.insert v (Set.delete w inVars)
-- TODO: in pretty code these asserts where also checked as part of the bool computation

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
prettyOperator pb (Equality v w) = let pVar = prettyVariable pb
  in pVar v ++ " = " ++ pVar w 
prettyOperator pb (And s) = "and " ++ prettySet (prettyNode pb) s
prettyOperator pb (Or s) = "or " ++ prettySet (prettyNode pb) s
prettyOperator pb (Exists v x) =
  "exists " ++ (prettyVariable pb) v ++ " . " ++ (prettyNode pb) x
prettyOperator pb (Project v x) =
  "proj " ++ (prettyVariable pb) v ++ " . " ++ (prettyNode pb) x
prettyOperator pb (Assign v w x) =  let pVar = prettyVariable pb
  in (prettyNode pb) x ++ "[" ++ pVar v ++ "/" ++ pVar w ++ "]"

prettyGraphPrinter :: PrettyOperatorBase r v n -> Graph r v n -> String
prettyGraphPrinter pb = BG.prettyGraphPrinter (prettyOperator pb)
                                              (prettyVariable pb)
                                              (prettyNode pb)

showGraph :: (Show r, Show v, Show n) => Graph r v n -> String
showGraph = prettyGraphPrinter $ PrettyOperatorBase show show show

prettyGraph :: (Pretty r, Pretty v, Pretty n) => Graph r v n -> String
prettyGraph = prettyGraphPrinter $ PrettyOperatorBase pretty pretty pretty

