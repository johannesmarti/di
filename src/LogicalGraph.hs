module LogicalGraph (
  Unfolding(..),
  mapUnfolding,
  BG.unfolding,
  outputVariablesFromUnfolding,
  Graph,
  BG.dataMap,
  BG.domain,
  BG.unfoldNode,
  successorsOfNode,
  BG.predecessorsOfNode,
  fromTupleList,
  LogicalGraph.fromSet,
  fromMap,
  subgraphOnSubset,
  subgraphOnPredicate,
  PrettyLogicalBase(..),
  prettyUnfolding,
  prettyGraph
) where

import Control.Exception (assert)

import Data.List (nub, intercalate)
import Data.Map.Strict as Map
import Data.Maybe
import Data.Set as Set

import Atom hiding (Atom)
import qualified Atom as At
import qualified BaseGraph as BG
import Pretty

data Unfolding r v n = Atom (At.Atom r v) | Equality v v |
                       And (Set n) | Or (Set n) |
                       Exists v n | Project v n | Assign v v n
  deriving (Eq,Ord)

mapUnfolding :: Ord m => (n -> m) -> Unfolding r v n -> Unfolding r v m
mapUnfolding f (Atom a) = Atom a
mapUnfolding f (Equality v w) = Equality v w
mapUnfolding f (And s) = And (Set.map f s)
mapUnfolding f (Or s) = Or (Set.map f s)
mapUnfolding f (Exists v n) = Exists v (f n)
mapUnfolding f (Project v n) = Project v (f n)
mapUnfolding f (Assign v w n) = Assign v w (f n)

nodesInUnfolding :: Ord n => Unfolding r v n -> Set n
nodesInUnfolding (And s) = s
nodesInUnfolding (Or s) = s
nodesInUnfolding (Exists v a) = Set.singleton a
nodesInUnfolding (Project v a) = Set.singleton a
nodesInUnfolding (Assign v w a) = Set.singleton a
nodesInUnfolding _ = Set.empty

inputVariablesFromUnfolding :: (Ord v, Ord n) =>
                               (n -> Set v) -> Unfolding r v n -> Maybe (Set v)
inputVariablesFromUnfolding outVars uf =
  BG.inputVariablesFromSuccessors outVars (nodesInUnfolding uf)

outputVariablesFromUnfolding :: (Ord v, Ord n) =>
                                (n -> Set v) -> Unfolding r v n -> Set v
outputVariablesFromUnfolding outVars uf =
  outputVariablesFromInputVariables uf . fromJust $
        inputVariablesFromUnfolding outVars uf

type Graph r v n = BG.Graph v (Unfolding r v n) n

successorsOfNode :: Ord n => Graph r v n -> n -> Set n
successorsOfNode = BG.successorsOfNode nodesInUnfolding

-- checking the consistency of the data

outputVariablesFromInputVariables :: Ord v => Unfolding r v n -> Set v -> Set v
outputVariablesFromInputVariables (Atom at) _ = Set.fromList $ arguments at
outputVariablesFromInputVariables (Equality v w) _ = Set.fromList $ [v, w]
outputVariablesFromInputVariables (And _) inVars = inVars
outputVariablesFromInputVariables (Or _) inVars = inVars
outputVariablesFromInputVariables (Exists v _) inVars = Set.delete v inVars
outputVariablesFromInputVariables (Project v _) inVars =
  assert (not $ v `Set.member` inVars) $ Set.insert v inVars
outputVariablesFromInputVariables (Assign v w _) inVars = 
  assert (not $ v `Set.member` inVars)
  assert (w `Set.member` inVars) $ Set.insert v (Set.delete w inVars)
-- TODO: in pretty code these asserts where also checked as part of the bool computation

-- TODO: Should also check that renamed, bound and projected variables make
-- sense. I am not yet sure how to do this well.
isCoherent :: (Ord v, Ord n) => Graph r v n -> Bool
isCoherent = BG.isCoherent nodesInUnfolding
                           outputVariablesFromInputVariables


-- creation

fromTupleList :: (Ord v, Ord n) => [(n, Unfolding r v n, Set v)]
                                   -> Graph r v n
fromTupleList tupleList = assert (isCoherent result) result where
  result = BG.fromTupleList nodesInUnfolding tupleList

fromSet :: (Ord v, Ord n) => Set n -> (n -> (Unfolding r v n, Set v))
                             -> Graph r v n
fromSet domSet f = assert (isCoherent result) result where
  result = BG.fromSet nodesInUnfolding domSet f

fromMap :: (Ord v, Ord n) => Map n (Unfolding r v n, Set v) -> Graph r v n
fromMap m = assert (isCoherent result) result where
  result = BG.fromMap nodesInUnfolding m

subgraphOnSubset :: (Ord v, Ord n) => Set n -> Graph r v n -> Graph r v n
subgraphOnSubset subset graph = assert (isCoherent result) result where
  result = BG.subgraphOnSubset filterUnfolding subset graph
  filterUnfolding uf = case uf of
                         And nodes -> And (nodes `Set.intersection` subset)
                         Or nodes  -> Or  (nodes `Set.intersection` subset)
                         _         -> uf

subgraphOnPredicate :: (Ord v, Ord n) => (n -> Bool) -> Graph r v n
                                         -> Graph r v n
subgraphOnPredicate inSubgraph graph =
  assert (isCoherent result) result where
    result = BG.subgraphOnPredicate filterUnfolding inSubgraph graph
    filterUnfolding uf = case uf of
                           And nodes -> And (Set.filter inSubgraph nodes)
                           Or nodes  -> Or  (Set.filter inSubgraph nodes)
                           _         -> uf


-- pretty printing

prettySet :: (e -> String) -> Set e -> String
prettySet prettyElement set =
  "{" ++ intercalate ", " (Prelude.map prettyElement (Set.toList set)) ++ "}"

data PrettyLogicalBase r v n = PrettyLogicalBase {
  prettyRelation :: r -> String,
  prettyVariable :: v -> String,
  prettyNode :: n -> String
}

prettyUnfolding :: PrettyLogicalBase r v n -> Unfolding r v n -> String
prettyUnfolding pb (Atom dpAtom) =
  prettyAtom (prettyRelation pb) (prettyVariable pb) dpAtom
prettyUnfolding pb (Equality v w) = let pVar = prettyVariable pb
  in pVar v ++ " = " ++ pVar w 
prettyUnfolding pb (And s) = "and " ++ prettySet (prettyNode pb) s
prettyUnfolding pb (Or s) = "or " ++ prettySet (prettyNode pb) s
prettyUnfolding pb (Exists v x) =
  "exists " ++ (prettyVariable pb) v ++ " . " ++ (prettyNode pb) x
prettyUnfolding pb (Project v x) =
  "proj " ++ (prettyVariable pb) v ++ " . " ++ (prettyNode pb) x
prettyUnfolding pb (Assign v w x) =  let pVar = prettyVariable pb
  in (prettyNode pb) x ++ "[" ++ pVar v ++ "/" ++ pVar w ++ "]"

prettyGraphPrinter :: PrettyLogicalBase r v n -> Graph r v n -> String
prettyGraphPrinter pb = BG.prettyGraphPrinter (prettyUnfolding pb)
                                              (prettyVariable pb)
                                              (prettyNode pb)

showGraph :: (Show r, Show v, Show n) => Graph r v n -> String
showGraph = prettyGraphPrinter $ PrettyLogicalBase show show show

prettyGraph :: (Pretty r, Pretty v, Pretty n) =>
                      Graph r v n -> String
prettyGraph = prettyGraphPrinter $ PrettyLogicalBase pretty pretty pretty

